{-# LANGUAGE DataKinds #-}
module Agda.LSP.Main (runAgdaLSP) where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad

import Control.Concurrent

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Data.HashMap.Strict (HashMap)
import Data.Aeson.Types
import Data.Default
import Data.Proxy

import GHC.Generics

import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Lens
import Language.LSP.Server

import System.Exit

import Agda.TypeChecking.Monad.Base
import Agda.Utils.Either (mapLeft)
import Agda.Utils.Lens
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Control.Exception (bracket)
import Agda.TypeChecking.Monad.Debug (reportSDoc)
import Agda.Interaction.Response.Base
import Agda.Utils.FileName
import System.IO
import qualified Agda.Interaction.Imports as Imp
import Agda.Interaction.Imports (parseSource)
import Data.Coerce (coerce)
import Agda.Interaction.FindFile (SourceFile(SourceFile))
import Agda.Compiler.Backend (setInteractionOutputCallback, resetState)
import Agda.LSP.Translation
import Agda.Interaction.Highlighting.JSON (jsonifyHighlightingInfo)
import qualified Data.Aeson.KeyMap as KeyMap

data LspConfig = LspConfig
  { lspHighlightingLevel :: HighlightingLevel
  }
  deriving (Show, Generic)

instance FromJSON HighlightingLevel
instance FromJSON LspConfig

initLspConfig :: LspConfig
initLspConfig = LspConfig
  { lspHighlightingLevel = NonInteractive
  }

data LspState = LspState
  { lspStateConfig  :: LanguageContextEnv LspConfig
  , lspStateWorkers :: MVar (HashMap NormalizedUri (MVar TCState))
  , lspStateSetup   :: TCM ()
  }

newtype WorkerM a = WorkerM { unWorkerM :: ReaderT LspState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader LspState)

instance MonadLsp LspConfig WorkerM where
  getLspEnv = asks lspStateConfig

syncOptions :: TextDocumentSyncOptions
syncOptions = TextDocumentSyncOptions
  { _openClose         = Just True
  , _change            = Just TextDocumentSyncKind_Incremental
  , _willSave          = Just False
  , _willSaveWaitUntil = Just False
  , _save              = Just (InR (SaveOptions (Just False)))
  }

runAgdaLSP :: TCM () -> TCM ()
runAgdaLSP setup = do
  liftIO do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering

  exc <- liftIO $ runServer ServerDefinition
    { defaultConfig    = initLspConfig
    , configSection    = "agda"
    , parseConfig      = \_ -> mapLeft Text.pack . parseEither parseJSON
    , onConfigChange   = const (pure ())
    , doInitialize     = lspInit setup
    , staticHandlers   = lspHandlers
    , interpretHandler = \state -> Iso (flip runReaderT state . unWorkerM) liftIO
    , options          = def
      { optTextDocumentSync = Just syncOptions
      }
    }
  liftIO case exc of
    0 -> exitSuccess
    n -> exitWith (ExitFailure n)

lspOutputCallback :: Uri -> LanguageContextEnv LspConfig -> InteractionOutputCallback
lspOutputCallback uri config = liftIO . runLspT config . \case
  Resp_RunningInfo _ s -> lspDebug s
  Resp_HighlightingInfo i rem method mod -> do
    let
      wrap toks = Object $ KeyMap.fromList
        [ ("data", toks)
        , ("uri", toJSON uri)
        ]
    sendNotification (SMethod_CustomMethod (Proxy :: Proxy "agda/pushTokens"))
      $ wrap
      $ toJSON
      $ aspectMapToTokens i
  _ -> pure ()

lspInit
  :: TCM ()
  -> LanguageContextEnv LspConfig
  -> a
  -> IO (Either ResponseError LspState)
lspInit setup config _ = do
  workers  <- newMVar mempty
  pure $ Right LspState
    { lspStateConfig  = config
    , lspStateWorkers = workers
    , lspStateSetup   = setup
    }

lspDebug :: MonadLsp cfg m => String -> m ()
lspDebug s = sendNotification SMethod_WindowLogMessage (LogMessageParams MessageType_Log (Text.pack s))

onTextDocumentOpen :: Handlers WorkerM
onTextDocumentOpen = notificationHandler SMethod_TextDocumentDidOpen \notif -> do
  workers <- asks lspStateWorkers
  let theUri = notif ^. params . textDocument . uri
  case uriToFilePath theUri of
    Just fp -> withRunInIO \run -> do
      let norm = toNormalizedUri theUri

      lock <- liftIO $ newMVar initState
      modifyMVar_ workers $ pure . HashMap.insert norm lock

      void . forkIO . run $ reloadUri theUri

    Nothing -> lspDebug $ "Refusing to load non-file:// URI " <> show theUri

onTextDocumentSaved :: Handlers WorkerM
onTextDocumentSaved = notificationHandler SMethod_TextDocumentDidSave \notif -> do
  lspDebug $ "Document saved, will reload:" <> show (notif ^. params . textDocument . uri)
  reloadUri (notif ^. params . textDocument . uri)

reloadUri :: Uri -> WorkerM ()
reloadUri uri = withIndefiniteProgress "Loading..." Nothing NotCancellable \progress -> withRunInIO \run -> run do
  sendNotification SMethod_TextDocumentPublishDiagnostics $ PublishDiagnosticsParams
    { _uri         = uri
    , _version     = Nothing
    , _diagnostics = []
    }

  void $ workTCM uri \fp -> do
    liftIO $ run $ progress "Resetting state"
    resetState
    liftIO $ run $ progress "Parsing source file"
    src <- parseSource fp
    liftIO $ run $ progress "Type checking"
    Imp.typeCheckMain Imp.TypeCheck src

  sendNotification (SMethod_CustomMethod (Proxy :: Proxy "agda/finishTokens"))
    $ Object $ KeyMap.singleton "uri" (toJSON uri)

runTCMWorker :: Uri -> TCState -> TCM a -> WorkerM (a, TCState)
runTCMWorker uri state cont = do
  setup <- asks lspStateSetup
  ctx <- asks lspStateConfig
  conf <- getConfig

  let
    wrap = locallyTC eHighlightingLevel (const (lspHighlightingLevel conf))

  (res, state') <- liftIO $ runTCM initEnv state do
    setup
    setInteractionOutputCallback (lspOutputCallback uri ctx)
    tryError (wrap cont) >>= \case
      Left err -> Left <$> errorToDiagnostic err
      Right x -> pure (Right x)

  case res of
    Left err -> do
      sendNotification SMethod_TextDocumentPublishDiagnostics $ PublishDiagnosticsParams
        { _uri         = uri
        , _version     = Nothing
        , _diagnostics = err
        }
      pure (undefined, state')
    Right x -> pure (x, state')

workTCM :: Uri -> (SourceFile -> TCM a) -> WorkerM a
workTCM uri cont = do
  let norm = toNormalizedUri uri
  locks <- asks lspStateWorkers
  lock <- (=<<) (HashMap.lookup norm) <$> liftIO (tryReadMVar locks)

  case (,) <$> lock <*> uriToFilePath uri of
    Just (lock, fp) -> do
      state <- liftIO $ takeMVar lock
      fp <- SourceFile <$> liftIO (absolute fp)
      (a, st) <- runTCMWorker uri state (cont fp)

      lspDebug $ "Type checking action finished for file " <> show uri <> ", will release lock"
      liftIO $ putMVar lock st
      lspDebug "lock released"

      pure a

    Nothing -> Prelude.error "TODO"

lspHandlers :: ClientCapabilities -> Handlers WorkerM
lspHandlers _ = mconcat
  [ onTextDocumentOpen
  , onTextDocumentSaved
  , notificationHandler SMethod_TextDocumentDidChange (const (pure ()))
  ]
