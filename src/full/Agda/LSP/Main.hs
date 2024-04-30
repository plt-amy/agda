{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ImplicitParams #-}
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
import Data.IORef

import GHC.Generics

import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types as Lsp
import Language.LSP.Protocol.Lens
import Language.LSP.Server

import System.Exit

import Agda.TypeChecking.Monad.Base
import Agda.Utils.Either (mapLeft)
import Agda.Utils.Lens
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Control.Exception (bracket, evaluate)
import Agda.TypeChecking.Monad.Debug (reportSDoc, reportSLn)
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
import qualified Agda.Utils.RangeMap as RangeMap
import Agda.Interaction.Highlighting.Precise
import Agda.Syntax.Position (noRange)
import Agda.LSP.Position (PosDelta, changeToDelta, updatePosition)
import Agda.Interaction.Highlighting.Common (toAtoms)
import Language.LSP.VFS (virtualFileText)
import qualified Data.Text.Lazy as TL
import Agda.Utils.Time (getCPUTime)
import Agda.TypeChecking.Pretty (pretty)
import Control.DeepSeq (NFData(rnf))

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

data Worker = Worker
  { workerUri  :: !NormalizedUri
    -- ^ URI managed by this worker.

  , workerFilePath :: FilePath
    -- ^ File path managed by this worker.

  , workerLoadedState :: !(MVar (IORef TCState))
    -- ^ Mutable variable for the “main” TC state in this worker.

  , workerThread      :: ThreadId
    -- ^ Thread for this worker.

  , workerTasks       :: Chan (Worker -> TCM ())

  , workerContext     :: LanguageContextEnv LspConfig

  , workerPosDelta    :: !(MVar PosDelta)
  }

data LspState = LspState
  { lspStateConfig  :: LanguageContextEnv LspConfig
  , lspStateWorkers :: MVar (HashMap NormalizedUri Worker)
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

  exc <- liftIO $ runServerWithHandles mempty mempty stdin stdout ServerDefinition
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

spawnOrGet :: Uri -> WorkerM Worker
spawnOrGet uri = withRunInIO \run -> case uriToFilePath uri of
  Just fp -> do
    state <- run ask
    let norm = toNormalizedUri uri
    modifyMVar (lspStateWorkers state) \workers -> case HashMap.lookup norm workers of
      Just worker -> pure (workers, worker)

      Nothing -> do
        lock   <- newMVar =<< newIORef initState
        deltas <- newMVar mempty
        chan   <- newChan

        rec
          wthread <- forkIO . forever $ do
            task <- readChan chan

            let
              report :: TCErr -> TCM ()
              report terr = do
                diag <- errorToDiagnostic terr
                liftIO . run $
                  sendNotification SMethod_TextDocumentPublishDiagnostics $ PublishDiagnosticsParams
                    { _uri         = uri
                    , _version     = Nothing
                    , _diagnostics = diag
                    }

              task' = do
                lspStateSetup state
                setInteractionOutputCallback (lspOutputCallback uri (lspStateConfig state))
                conf <- liftIO (run getConfig)
                locallyTC eHighlightingLevel (const (lspHighlightingLevel conf)) do
                  task worker `catchError` report

            withMVar lock \state -> do
              unTCM task' state initEnv

          let
            worker = Worker
              { workerUri         = norm
              , workerFilePath    = fp
              , workerLoadedState = lock
              , workerThread      = wthread
              , workerTasks       = chan
              , workerContext     = lspStateConfig state
              , workerPosDelta    = deltas
              }

        run $ lspDebug $ "Spawned worker " <> show (workerThread worker) <> " to manage file " <> fp
        pure (HashMap.insert norm worker workers, worker)

  Nothing -> Prelude.error "TODO"

type Task = (?worker :: Worker) => TCM ()

runAtURI :: Uri -> Task -> WorkerM ()
runAtURI uri task = do
  Worker{workerTasks = chan} <- spawnOrGet uri
  liftIO $ writeChan chan (\worker -> let ?worker = worker in task)

onTextDocumentOpen :: Handlers WorkerM
onTextDocumentOpen = notificationHandler SMethod_TextDocumentDidOpen \notif ->
  runAtURI (notif ^. params . textDocument . uri) do
  reportSLn "lsp.lifecycle" 10 "document opened"
  reloadURI

onTextDocumentSaved :: Handlers WorkerM
onTextDocumentSaved = notificationHandler SMethod_TextDocumentDidSave \notif ->
  runAtURI (notif ^. params . textDocument . uri) do reloadURI

onTextDocumentChange :: Handlers WorkerM
onTextDocumentChange = notificationHandler SMethod_TextDocumentDidChange \notif ->
  runAtURI (notif ^. params . textDocument . uri) do
  reportSLn "lsp.lifecycle" 10 "document changed"

  liftIO $ modifyMVar_ (workerPosDelta ?worker) \old -> do
    let new = foldMap changeToDelta (notif ^. params . contentChanges)
    -- Important: mappend for PosDelta is *applicative order*, so this
    -- means "first apply the old diff, then apply the new diff".
    new `seq` pure (new <> old)

notifyTCM
  :: forall (m :: Method 'ServerToClient 'Notification). (?worker :: Worker)
  => SServerMethod m -> MessageParams m -> TCM ()
notifyTCM notif param = liftIO . runLspT (workerContext ?worker) $ sendNotification notif param

requestTCM
  :: forall (m :: Method 'ServerToClient 'Request). (?worker :: Worker)
  => SServerMethod m -> MessageParams m -> (Either ResponseError (MessageResult m) -> Task) -> TCM ()
requestTCM notif param handle = liftIO . runLspT (workerContext ?worker) $ void $
  sendRequest notif param \resp -> liftIO do
    writeChan (workerTasks ?worker) (\worker -> let ?worker = worker in handle resp)

diagnoseTCM :: (?worker :: Worker) => [Diagnostic] -> TCM ()
diagnoseTCM diag = notifyTCM SMethod_TextDocumentPublishDiagnostics $ PublishDiagnosticsParams
  { _uri         = fromNormalizedUri (workerUri ?worker)
  , _version     = Nothing
  , _diagnostics = diag
  }

reloadURI :: Task
reloadURI = do
  resetState
  liftIO $ modifyMVar_ (workerPosDelta ?worker) \_ -> pure mempty
  diagnoseTCM []

  sf <- SourceFile <$> liftIO (absolute (workerFilePath ?worker))
  src <- parseSource sf
  void $ Imp.typeCheckMain Imp.TypeCheck src
  requestTCM SMethod_WorkspaceSemanticTokensRefresh Nothing (const (pure ()))

requestHandlerTCM
  :: forall (m :: Method 'ClientToServer 'Request). SMethod m
  -> (TRequestMessage m -> Uri)
  -> (TRequestMessage m -> (Either ResponseError (MessageResult m) -> TCM ()) -> Task)
  -> Handlers WorkerM
requestHandlerTCM method uri cont = requestHandler method \req res -> withRunInIO \run -> run do
  runAtURI (uri req) $ cont req \m -> liftIO (run (res m))

provideSemanticTokens :: Handlers WorkerM
provideSemanticTokens =
  requestHandlerTCM SMethod_TextDocumentSemanticTokensFull (view (params . textDocument . uri)) \req res -> do
  info <- useTC stSyntaxInfo

  delta <- liftIO $ readMVar (workerPosDelta ?worker)
  let tokens = aspectMapToTokens delta info
  reportSLn "lsp.highlighting.semantic" 10 "returning semantic tokens"

  case encodeTokens agdaTokenLegend (relativizeTokens tokens) of
    Left  e    -> res $ Right (InR Lsp.Null)
    Right ints -> res $ Right $ InL (SemanticTokens Nothing ints)

onInitialized :: Handlers WorkerM
onInitialized = notificationHandler SMethod_Initialized \notif -> do
  sendNotification (SMethod_CustomMethod (Proxy :: Proxy "agda/highlightingInit"))
    $ Object $ KeyMap.singleton "legend" (toJSON agdaTokenLegend)

lspHandlers :: ClientCapabilities -> Handlers WorkerM
lspHandlers _ = mconcat
  [ onTextDocumentOpen
  , onTextDocumentSaved
  -- , onTextDocumentClosed
  , onInitialized
  , onTextDocumentChange
  , notificationHandler SMethod_WorkspaceDidChangeConfiguration (const (pure ()))
  , provideSemanticTokens
  ]
  -- , provideSemanticTokens
  -- , onInitialized
  -- ]
