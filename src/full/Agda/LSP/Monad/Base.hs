{-# LANGUAGE RoleAnnotations #-}
module Agda.LSP.Monad.Base where

import Prelude hiding (null)

import Control.Concurrent.MVar
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Except
import Control.Applicative (liftA2)
import Control.Concurrent
import Control.Monad

import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import Data.HashMap.Strict (HashMap)
import Data.IntMap.Strict (IntMap)
import Data.Dynamic

import Data.Aeson
import Data.IORef
import Data.List (find)

import GHC.Generics

import qualified Language.LSP.Protocol.Types as Lsp
import Language.LSP.Protocol.Types (NormalizedUri, Uri, fromNormalizedUri)
import Language.LSP.Server

import Agda.Syntax.Common

import Agda.TypeChecking.Monad

import qualified Agda.Utils.BiMap as BiMap

import Agda.LSP.Position
import Agda.Interaction.FindFile (SourceFile(..))
import Agda.Interaction.Options
import Agda.Utils.FileName (absolute)
import Agda.Utils.Null
import Data.String
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.Utils.Functor
import Agda.Utils.Maybe (fromMaybe)
import Agda.LSP.Translation (rangeContains)

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
  { workerUri         :: !NormalizedUri
    -- ^ URI managed by this worker.

  , workerFilePath    :: FilePath
    -- ^ File path managed by this worker.

  , workerLoadedState :: !(MVar (IORef TCState))
    -- ^ Mutable variable for the “main” TC state in this worker.

  , workerThread      :: ThreadId
    -- ^ Thread for this worker.

  , workerTasks       :: Chan (Task ())

  , workerContext     :: LanguageContextEnv LspConfig

  , workerOptions     :: CommandLineOptions

  , workerPosDelta    :: !(MVar PosDelta)
  }

data LspState = LspState
  { lspStateConfig  :: LanguageContextEnv LspConfig
  , lspStateWorkers :: MVar (HashMap NormalizedUri Worker)
  , lspStateOptions :: CommandLineOptions
  }

newtype WorkerM a = WorkerM { unWorkerM :: ReaderT LspState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader LspState)

instance MonadLsp LspConfig WorkerM where
  getLspEnv = asks lspStateConfig

newtype Task a = Task { unTask :: ReaderT Worker TCM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTCM, HasOptions, MonadTCEnv, MonadTCState, MonadDebug, ReadTCState, HasConstInfo, MonadFail, HasBuiltins, MonadError TCErr, MonadTrace, MonadReduce, MonadFresh NameId, MonadInteractionPoints, MonadStConcreteNames, MonadAddContext, PureTCM)

instance MonadUnliftIO Task where
  withRunInIO k = Task $ ReaderT \worker -> TCM \state env -> k \task -> unTCM (runReaderT (unTask task) worker) state env

instance MonadLsp LspConfig Task where
  getLspEnv = Task (asks workerContext)

instance IsString a => IsString (Task a) where
  fromString s = pure (fromString s)

instance {-# OVERLAPPABLE #-} (Semigroup a) => Semigroup (Task a) where
  (<>) = liftA2 (<>)

-- | Strict (non-shortcut) monoid.
instance {-# OVERLAPPABLE #-} (Semigroup a, Monoid a) => Monoid (Task a) where
  mempty  = pure mempty
  mappend = (<>)
  mconcat = mconcat <.> sequence

instance {-# OVERLAPPABLE #-} Null a => Null (Task a) where
  empty = return empty
  null  = __IMPOSSIBLE__

getDelta :: Task PosDelta
getDelta = liftIO . readMVar =<< Task (asks workerPosDelta)

modifyDelta :: (PosDelta -> PosDelta) -> Task ()
modifyDelta k = do
  delta <- Task (asks workerPosDelta)
  liftIO $ modifyMVar_ delta (pure . k)

theSourceFile :: Task SourceFile
theSourceFile = do
  file <- Task (asks workerFilePath)
  SourceFile <$> liftIO (absolute file)

theURI :: Task Uri
theURI = Task (asks (fromNormalizedUri . workerUri))

getInitialOptions :: Task CommandLineOptions
getInitialOptions = Task (asks workerOptions)

findInteractionPoint :: Lsp.Position -> Task (Maybe (InteractionId, InteractionPoint))
findInteractionPoint pos = do
  ii <- useR stInteractionPoints
  delta <- getDelta
  let
    go (_, ip) = not (ipSolved ip) && rangeContains delta pos (ipRange ip)
  pure $ find go (BiMap.toList ii)

withPosition :: Lsp.Position -> (InteractionPoint -> Task a) -> Task (Maybe a)
withPosition pos cont = do
  containing <- findInteractionPoint pos
  case containing of
    Just (iid, ip) -> Just <$> withInteractionId iid (cont ip)
    Nothing -> pure Nothing
