{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NondecreasingIndentation #-}
module Agda.LSP.Main (runAgdaLSP) where

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Except
import Control.Concurrent
import Control.Exception
import Control.Monad

import qualified Data.Text.Utf16.Rope.Mixed as Rope
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text.Lines as TextLines
import qualified Data.Text.Lazy as TL
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Strict.Tuple (Pair(..))
import Data.Aeson.Types as Aeson
import Data.Foldable
import Data.Default
import Data.Proxy
import Data.IORef
import Data.Coerce
import Data.Maybe
import Data.List (find, sortOn, intercalate)

import GHC.Generics

import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types as Lsp
import Language.LSP.Protocol.Lens
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText, rangeLinesFromVfs, VirtualFile, file_text)

import System.Exit
import System.IO

import qualified Text.PrettyPrint.Annotated.HughesPJ as Ppr
import qualified Text.PrettyPrint.Annotated as Ppr

import qualified Agda.Syntax.Concrete as C
import qualified Agda.Syntax.Position as Agda

import Agda.Syntax.Translation.InternalToAbstract (Reify(reify), reifyUnblocked)
import Agda.Syntax.Translation.AbstractToConcrete
import Agda.Syntax.Abstract.Pretty
import Agda.Syntax.Concrete.Pretty () -- Instances only
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.Syntax.Scope.Base
import Agda.Syntax.Concrete (Module(modDecls))
import Agda.Syntax.Position
import Agda.Syntax.Internal

import Agda.TypeChecking.Substitute
import Agda.TypeChecking.Telescope
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Reduce
import Agda.TypeChecking.Monad as I

import Agda.LSP.Translation
import Agda.LSP.Position
import Agda.LSP.Goal

import qualified Agda.Interaction.Imports as Imp
import qualified Agda.Interaction.BasicOps as B
import Agda.Interaction.Highlighting.Precise
import Agda.Interaction.Highlighting.Common (toAtoms)
import Agda.Interaction.Response.Base
import Agda.Interaction.FindFile (SourceFile(SourceFile))
import Agda.Interaction.BasicOps (getWarningsAndNonFatalErrors, normalForm)
import Agda.Interaction.JSONTop () -- Instances only

import qualified Agda.Utils.RangeMap as RangeMap
import qualified Agda.Utils.BiMap as BiMap

import Agda.Utils.Impossible
import Agda.Utils.FileName
import Agda.Utils.Either
import Agda.Utils.Lens
import Agda.Syntax.Parser (runPMIO)
import Agda.Mimer.Mimer as Mimer
import qualified Agda.Syntax.Parser as P
import qualified Agda.Utils.Maybe.Strict as Strict
import qualified Agda.Utils.RangeMap as RangeMap
import Agda.Utils.RangeMap (RangeMap)
import Agda.Syntax.Parser.Tokens (Token(TokSymbol), Symbol (SymQuestionMark))
import Agda.Syntax.Common (InteractionId)
import Agda.Syntax.Fixity (Precedence(TopCtx))
import Agda.Interaction.Base (Rewrite(AsIs), UseForce (WithoutForce))

import Agda.LSP.Monad.Base
import Agda.LSP.Commands
import Agda.LSP.Output (Printed(Printed), renderToJSON)
import Agda.Interaction.InteractionTop (highlightExpr)
import Agda.Interaction.Highlighting.Range (rangeToRange)
import Data.Char (isSpace)

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
      , optDocumentOnTypeFormattingTriggerCharacters = Just ('?' :| [])
      , optExecuteCommandCommands = Just (map commandName [minBound..maxBound])
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

spawnOrGet :: Uri -> WorkerM (Maybe Worker)
spawnOrGet uri = withRunInIO \run -> case uriToFilePath uri of
  Just fp -> do
    state <- run ask
    let norm = toNormalizedUri uri
    modifyMVar (lspStateWorkers state) \workers -> case HashMap.lookup norm workers of
      Just worker -> pure (workers, Just worker)

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
                  runReaderT (unTask task) worker `catchError` report

            withMVar lock \state -> do
              unTCM task' state initEnv
                `catch` \(e :: SomeException) -> run (lspDebug (show e))

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
        pure (HashMap.insert norm worker workers, Just worker)

  Nothing -> pure Nothing

runAtURI :: Uri -> Task a -> WorkerM ()
runAtURI uri task = spawnOrGet uri >>= \case
  Just Worker{workerTasks = chan} -> liftIO $ writeChan chan (void task)
  Nothing -> lspDebug $ "URI is not a file: " <> show uri

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
  reportSLn "lsp.lifecycle" 10 $ "document changed\n" <> unlines (map show (notif ^. params . contentChanges))

  modifyDelta \old -> do
    let new = foldMap changeToDelta (reverse (notif ^. params . contentChanges))
    -- Important: mappend for PosDelta is *applicative order*, so this
    -- means "first apply the old diff, then apply the new diff".
    new `seq` (new <> old)

onTextDocumentClosed :: Handlers WorkerM
onTextDocumentClosed = notificationHandler SMethod_TextDocumentDidClose \notif ->
  withRunInIO \run -> do
    let uri' = notif ^. params . textDocument . uri
    case uriToFilePath uri' of
      Just fp -> do
        state <- run ask
        let norm = toNormalizedUri uri'
        worker <- modifyMVar (lspStateWorkers state) \workers -> pure (HashMap.delete norm workers, HashMap.lookup norm workers)
        traverse_ (killThread . workerThread) worker

      Nothing -> pure ()

diagnoseTCM :: [Diagnostic] -> Task ()
diagnoseTCM diag = do
  uri <- theURI
  sendNotification SMethod_TextDocumentPublishDiagnostics $ PublishDiagnosticsParams
    { _uri         = uri
    , _version     = Nothing
    , _diagnostics = diag
    }

toDiagnostic :: forall a. (PrettyTCM a, Agda.HasRange a) => a -> TCM Lsp.Diagnostic
toDiagnostic x = do
  msg <- Ppr.render <$> prettyTCM x
  pure $ Lsp.Diagnostic
    { _range              = toLsp (Agda.getRange x)
    , _severity           = Just Lsp.DiagnosticSeverity_Error
    , _code               = Nothing
    , _codeDescription    = Nothing
    , _source             = Just "agda"
    , _message            = Text.pack (msg <> "\n" <> show (toLsp (Agda.getRange x)))
    , _tags               = Nothing
    , _relatedInformation = Nothing
    , _data_              = Nothing
    }

reloadURI :: Task ()
reloadURI = do
  liftTCM resetState
  modifyDelta (const mempty)
  diagnoseTCM []

  sf <- theSourceFile
  src <- liftTCM $ Imp.parseSource sf
  cr <- liftTCM $ Imp.typeCheckMain Imp.TypeCheck src
  void $ sendRequest SMethod_WorkspaceSemanticTokensRefresh Nothing (const (pure ()))

  unless (null (Imp.crWarnings cr)) do
    WarningsAndNonFatalErrors warn err <- liftTCM $ getWarningsAndNonFatalErrors
    warn <- traverse (liftTCM . toDiagnostic) warn
    err <- traverse (liftTCM . toDiagnostic) err
    diagnoseTCM (warn ++ err)

  reportSDoc "lsp.lifecycle" 10 $ "got warnings: " <+> prettyTCM (Imp.crWarnings cr)

  -- ii <- useR stInteractionPoints
  -- let
  --   edits = BiMap.elems ii >>= \ip -> do
  --     guard (isQuestion ip)
  --     pure $! TextEdit (toLsp (ipRange ip)) "{! !}"

  --   edit = ApplyWorkspaceEditParams
  --     { _label = Just "Question marks"
  --     , _edit = WorkspaceEdit
  --       { _documentChanges   = Nothing
  --       , _changeAnnotations = Nothing
  --       , _changes           = Just (Map.singleton (fromNormalizedUri (workerUri ?worker)) edits)
  --       }
  --     }

  -- requestTCM SMethod_WorkspaceApplyEdit edit (const (pure ()))

isQuestion :: InteractionPoint -> Bool
isQuestion InteractionPoint{ipRange = r}
  | Just i <- rangeToInterval r, posPos (iEnd i) == posPos (iStart i) + 1 = True
isQuestion _ = False

requestHandlerTCM
  :: forall (m :: Method 'ClientToServer 'Request) a. SMethod m
  -> (TRequestMessage m -> Uri)
  -> (TRequestMessage m -> (Either ResponseError (MessageResult m) -> Task ()) -> Task a)
  -> Handlers WorkerM
requestHandlerTCM method uri cont = requestHandler method \req res -> withRunInIO \run -> run do
  runAtURI (uri req) $ void $ cont req \m -> liftIO (run (res m))

provideSemanticTokens :: Handlers WorkerM
provideSemanticTokens =
  requestHandlerTCM SMethod_TextDocumentSemanticTokensFull (view (params . textDocument . uri)) \req res -> do
  info <- useTC stSyntaxInfo

  delta <- getDelta
  let tokens = aspectMapToTokens delta info
  reportSLn "lsp.highlighting.semantic" 10 $ "returning " <> show (Prelude.length tokens) <> " semantic tokens"

  case encodeTokens agdaTokenLegend (relativizeTokens (sortOn (\x -> (x ^. line, x ^. startChar)) tokens)) of
    Left  e    -> res $ Right (InR Lsp.Null)
    Right ints -> res $ Right $ InL (SemanticTokens Nothing ints)

onInitialized :: Handlers WorkerM
onInitialized = notificationHandler SMethod_Initialized \notif -> do
  sendNotification (SMethod_CustomMethod (Proxy :: Proxy "agda/highlightingInit"))
    $ Object $ KeyMap.singleton "legend" (toJSON agdaTokenLegend)

-- | Get a substring of a file based on an Lsp range.
getFileSubstring :: VirtualFile -> Lsp.Range -> Text.Text
getFileSubstring file (Lsp.Range (Lsp.Position sL sC) (Lsp.Position eL eC)) =
  let
    rope = file ^. file_text
    (_, prefix) = Rope.charSplitAtPosition (TextLines.Position (fromIntegral sL) (fromIntegral sC)) rope

    len = if sL == eL then eC - sC else eC
    (main, _) = Rope.charSplitAtPosition (TextLines.Position (fromIntegral (eL - sL)) (fromIntegral len)) prefix
  in
  Rope.toText main

-- | Get the current/latest range of the current interaction point.
interactionRange :: PosDelta -> InteractionPoint -> Maybe Lsp.Range
interactionRange delta ip = rangeToInterval (ipRange ip) >>= updatePosition delta . toLsp

-- | Get the contents of an interaction point, if still present in the document.
interactionContents :: VirtualFile -> Lsp.Range -> Maybe (Lsp.Range, Text.Text)
interactionContents file range = do
  contents <- Text.stripPrefix "{!" <=< Text.stripSuffix "!}" $ getFileSubstring file range
  let (spaces, text) = Text.span isSpace contents

  pure
    ( range & (start . character) %~ (+ (2 + fromIntegral (Text.length spaces)))
    , Text.strip text
    )

printedTerm :: Rewrite -> Term -> TCM C.Expr
printedTerm rewr ty = do
  ty  <- normalForm rewr ty
  abstractToConcreteCtx TopCtx =<< reify ty

fillQuery :: Query a -> Task a
fillQuery (Query_GoalAt pos) = do
  iis <- useR stInteractionPoints
  delta <- getDelta
  forM_ (BiMap.toList iis) \(id, ip) -> unless (ipSolved ip) do
    reportSLn "lsp.give" 10 $ unlines
      [ "point " <> show id
      , show (toLsp (ipRange ip))
      , show (updatePosition delta (toLsp (ipRange ip) ^. start))
      , show (updatePosition delta (toLsp (ipRange ip) ^. end))
      , show (downgradePosition delta pos)
      ]
  fmap fst <$> findInteractionPoint pos

fillQuery Query_AllGoals = do
  iis <- useR stInteractionPoints
  forM (BiMap.toList iis) \(id, ip) -> withInteractionId id do
    mi <- lookupInteractionId id
    ty <- fmap Printed . liftTCM . printedTerm AsIs . unEl =<< getMetaTypeInContext mi
    pure Goal
      { goalId    = id
      , goalType  = ty
      , goalRange = toLsp (ipRange ip)
      }

fillQuery (Query_GoalInfo iid) = withInteractionId iid do
  mi <- lookupInteractionId iid
  ip <- lookupInteractionPoint iid

  let
    mkVar :: ContextEntry -> Task (Maybe Local)
    mkVar Dom{ domInfo = ai, unDom = (name, t) } = do
      -- if shouldHide ai name then return Nothing else Just <$> do
        let n = nameConcrete name
        x <- abstractToConcrete_ name
        let s = C.isInScope x
        -- ty <- allocForeignCl t
        con <- abstractToConcreteCtx TopCtx =<< reify (unEl t)
        pure $ Just Local
          { localName        = Printed n
          , localReifiedName = Printed x
          , localType        = Printed con
          }

  ctx <- getContext
  let locals = zipWith raise [1..] ctx
  gty <- fmap Printed . liftTCM . printedTerm AsIs . unEl =<< getMetaTypeInContext mi
  locals <- catMaybes <$> forM locals mkVar
  pure GoalInfo
    { goalGoal    = Goal iid (toLsp (ipRange ip)) gty
    , goalContext = locals
    }

goal :: Handlers WorkerM
goal = requestHandler (SMethod_CustomMethod (Proxy :: Proxy "agda/query")) \req res -> withRunInIO \run -> run do
  case fromJSON @SomeQuery (req ^. params) of
    Success q@(SomeQuery uri query) -> runAtURI uri do
      reportSLn "lsp.query" 10 $ show q
      out <- tryError (fillQuery query)
      case out of
        Left err  -> liftIO . run . res $ Left $ ResponseError (InL LSPErrorCodes_RequestFailed) "Request failed" Nothing
        Right out -> liftIO . run . res $ Right $ toJSON out
    Aeson.Error e -> liftIO . run . res $ Left $ ResponseError (InL LSPErrorCodes_RequestFailed) (Text.pack e) Nothing
    -- pure ()

namedCompletionItem :: Text.Text -> CompletionItem
namedCompletionItem !name = CompletionItem
  { _textEditText        = Nothing
  , _sortText            = Nothing
  , _preselect           = Nothing
  , _labelDetails        = Nothing
  , _insertTextFormat    = Nothing
  , _insertText          = Nothing
  , _filterText          = Nothing
  , _documentation       = Nothing
  , _deprecated          = Nothing
  , _commitCharacters    = Nothing
  , _insertTextMode      = Nothing
  , _textEdit            = Nothing
  , _additionalTextEdits = Nothing
  , _command             = Nothing
  , _detail              = Nothing
  , _kind                = Nothing
  , _label               = name
  , _data_               = Nothing
  , _tags                = Nothing
  }

localCompletionItem :: MonadTCM m => Int -> Dom' Term (Name, Type) -> m (Maybe CompletionItem)
localCompletionItem ix var@Dom{unDom = (name, ty)} = liftTCM $ runMaybeT do
  ty <- Text.pack . Ppr.render <$> lift (prettyATop =<< reify (unEl (raise (ix + 1) ty)))

  concrete <- lift (abstractToConcrete_ name)
  guard (C.InScope == C.isInScope concrete)

  name <- Text.pack . Ppr.renderStyle (Ppr.Style Ppr.OneLineMode 0 0) <$> lift (prettyTCM name)

  pure $! namedCompletionItem name
    & sortText     ?~ "00_" <> Text.pack (show ix)
    & labelDetails ?~ CompletionItemLabelDetails (Just (" : " <> ty)) Nothing
    & kind         ?~ CompletionItemKind_Variable

headMatches :: Maybe Type -> Type -> TCM Bool
headMatches Nothing _ = pure True
headMatches (Just t) t' = do
  TelV ctx t' <- telView t'
  addContext ctx do
  t' <- fmap unEl <$> reduceB t'
  t <- reduce (raise (Prelude.length ctx) t)
  case (ignoreBlocking t', unEl t) of
    (Var{}, _)   -> pure True
    (_, MetaV{}) -> pure True
    (MetaV{}, _) -> pure True
    (Def q _, _)
      | Blocked{} <- t'    -> pure True
      | Def q' _ <- unEl t -> pure $! q == q'
    (Sort{},  Sort{})  -> pure True
    (Level{}, Level{}) -> pure True
    _ -> pure False

definedCompletionItem :: Maybe Type -> QName -> Task (Maybe CompletionItem)
definedCompletionItem _ qnm | isExtendedLambdaName qnm = pure Nothing
definedCompletionItem _ qnm | isAbsurdLambdaName qnm = pure Nothing
definedCompletionItem want qnm = getConstInfo qnm >>= \def -> runMaybeT do
  name <- Text.pack . Ppr.render <$> lift (prettyTCM qnm)

  let
    defKind = case theDef def of
      Axiom{}                             -> Just CompletionItemKind_Constant
      I.Datatype{}                        -> Just CompletionItemKind_Enum
      I.Record{}                          -> Just CompletionItemKind_Struct
      I.Function{funProjection = Right _} -> Just CompletionItemKind_Field
      I.Function{}                        -> Just CompletionItemKind_Function
      I.Primitive{}                       -> Just CompletionItemKind_Function
      I.Constructor{}                     -> Just CompletionItemKind_EnumMember
      _                                   -> Nothing

  guard =<< liftTCM (headMatches want (defType def))
  ty <- Text.pack . Ppr.render <$> lift (inTopContext (prettyATop =<< reify (defType def)))

  pure $! namedCompletionItem name
    & labelDetails ?~ CompletionItemLabelDetails (Just (" : " <> ty)) Nothing
    & kind         .~ defKind

completion :: Handlers WorkerM
completion = requestHandlerTCM SMethod_TextDocumentCompletion (view (params . textDocument . uri)) \req res ->
  withPosition (req ^. params . position) \ip -> do
    ctx  <- getContext
    comp <- traverse (uncurry localCompletionItem) (zip [0..] ctx)

    want  <- traverse getMetaTypeInContext (ipMeta ip)
    comp' <- traverse (definedCompletionItem want) . Set.toList =<< fmap (^. scopeInScope) getScope

    reportSLn "lsp.completion" 10 $ show req
    res (Right (InL (catMaybes comp ++ catMaybes comp')))


runLspTCM :: (?worker :: Worker) => NormalizedUri -> TCM (Maybe VirtualFile)
runLspTCM uri = liftIO $ runLspT (workerContext ?worker) (getVirtualFile uri)

onTypeFormatting :: Handlers WorkerM
onTypeFormatting = requestHandler SMethod_TextDocumentOnTypeFormatting \req res -> do
  text <- fmap virtualFileText <$> getVirtualFile (toNormalizedUri (req ^. params . textDocument . uri))
  case text of
    Just text -> do
      let
        linum = req ^. params . position . line
        l = Text.lines text !! fromIntegral linum

        edit (TokSymbol SymQuestionMark ival)
          | posPos (iEnd ival) == posPos (iStart ival) + 1
          = pure $ TextEdit {_newText="{! !}", _range=toLsp ival}
        edit _ = []

      (toks, _) <- runPMIO $ P.parsePosString P.tokensParser (Pn Strict.Nothing 0 (fromIntegral linum + 1) 1) $ Text.unpack l
      case toks of
        Right (toks, _) -> res $ Right $ InL $ toks >>= edit
        Left  _         -> res $ Right $ InR Lsp.Null

    Nothing -> pure ()

goToDefinition :: ClientCapabilities -> Handlers WorkerM
goToDefinition caps = requestHandlerTCM SMethod_TextDocumentDefinition (view (params . textDocument . uri)) \req res -> do
  reportSLn "lsp.definition" 10 $ show req

  info <- useTC stSyntaxInfo
  delta <- getDelta
  tc <- getTCState

  let currentAspect = findAspect delta (req ^. params . position) info
  res case currentAspect >>= definitionSite of
    Just (DefinitionSite mod pos _ _)
      | Just path <- tc ^. stModuleToSource . key mod
      , Just info <- tc ^. stVisitedModules . key mod
      , Just (_, RangeMap.PairInt (_ :!: node)) <- Map.lookupLE pos . RangeMap.rangeMap . iHighlighting . miInterface $ info
      , aspectRange node /= noRange
      ->
        let
          srcUri = Lsp.filePathToUri (filePath path)
          -- TODO: upgrade position if in the current file.
          srcPos = toLsp (aspectRange node)
        in Right $
          if fromMaybe False ((caps ^. textDocument) >>= view declaration >>= view linkSupport)
          then
            let
              link = Lsp.LocationLink
                { _originSelectionRange = toLsp . aspectRange <$> currentAspect
                , _targetUri = srcUri
                , _targetRange = srcPos
                , _targetSelectionRange = srcPos
                }
            in InR . InL $ [DefinitionLink link]
          else InL . Definition . InL $ Lsp.Location srcUri srcPos
    _ -> notFound

  where
    notFound = Right . InR . InR $ Lsp.Null

-- | Find an aspect at the specified position.
findAspect :: PosDelta -> Lsp.Position -> RangeMap Aspects -> Maybe Aspects
findAspect delta position =
  -- TODO: This is currently a linear search. Ideally we'd be able to map
  -- positions to byte offsets directly, which'd make this much more efficient.
  -- Alternatively we could do a binary search over the aspect map, we just need
  -- to ensure that all elements have a range.
  find (rangeContains delta position . aspectRange) . map snd . RangeMap.toList

highlightReferences :: Handlers WorkerM
highlightReferences = requestHandlerTCM SMethod_TextDocumentDocumentHighlight (view (params . textDocument . uri)) \req res -> do
  reportSLn "lsp.documentHighlight" 10 $ show req

  info <- useTC stSyntaxInfo
  delta <- getDelta

  let currentAspect = findAspect delta (req ^. params . position) info
  res case currentAspect >>= definitionSite of
    Nothing -> Right . InR $ Lsp.Null
    Just s@(DefinitionSite mod pos _ _) -> Right . InL
      . nubOrd
      . concatMap (mapMaybe (fmap makeHighlight . updatePosition delta . toLsp) . rangeIntervals . aspectRange . snd)
      . filter (\(_, x) -> elem s (definitionSite x))
      $ RangeMap.toList info

  where makeHighlight r = DocumentHighlight r (Just DocumentHighlightKind_Read)

getCodeActions :: Handlers WorkerM
getCodeActions = requestHandlerTCM SMethod_TextDocumentCodeAction (view (params . textDocument . uri)) \req res -> do
  let fileUri = req ^. params . textDocument . uri
  reportSLn "lsp.codeAction" 10 $ show req

  ii <- useR stInteractionPoints
  delta <- getDelta
  virtualFile <- getVirtualFile $ toNormalizedUri fileUri

  let
    reqRange@(Lsp.Range reqStart reqEnd) = req ^. params . range

    resolveInteraction ip = do
      range <- interactionRange delta ip
      (_, contents) <- virtualFile >>= flip interactionContents range
      pure (range, not (Text.null contents))

    -- Determine if this interaction point overlaps with the provided range
    -- at all
    containing =
      filter (\(Lsp.Range iS iE, _) -> iE >= reqStart && iS <= reqEnd)
        . mapMaybe resolveInteraction
        $ BiMap.elems ii

  res . Right $ case containing of
    [] -> InL []
    [(range, nonEmpty)] -> InL $
      [ InR $ makeCodeAction "Run proof search on this goal" (Command_Auto fileUri (range ^. start)) ] ++
      -- TODO: Refine
      [ InR $ makeCodeAction "Fill goal with current contents" (Command_Give fileUri (range ^. start)) | nonEmpty ]
    _ -> InL []

  where
      makeCodeAction title command =
        CodeAction
        { _title = title
        , _kind = Just CodeActionKind_QuickFix
        , _diagnostics = Nothing
        , _isPreferred = Nothing
        , _disabled = Nothing
        , _edit = Nothing
        , _command = Just (toCommand title command)
        , _data_ = Nothing
        }

-- | Handle a command inside the TCM monad.
commandHandlerTCM
  :: Uri
  -> (Either ResponseError (Value |? Null) -> WorkerM ())
  -> ((forall a. WorkerM a -> IO a)
    -> Task (Either ResponseError (Value |? Null)))
  -> WorkerM ()
commandHandlerTCM uri res cont = withRunInIO \run -> run $
  runAtURI uri (cont run >>= (liftIO . run . res))

-- | Handle a command at an interaction point.
commandHandlerInteractionPoint
  :: Uri
  -> Lsp.Position
  -> (Either ResponseError (Value |? Null) -> WorkerM ())
  -> ((forall a. WorkerM a -> IO a)
    -> InteractionId -> InteractionPoint -> Lsp.Range -> Lsp.Range -> Text.Text
    -> Task (Either ResponseError (Value |? Null)))
  -> WorkerM ()
commandHandlerInteractionPoint uri pos res cont = commandHandlerTCM uri res \run -> do
    pos <- findInteractionPoint pos
    virtualFile <- liftIO . run . getVirtualFile $ toNormalizedUri uri
    delta <- getDelta

    case pos of
      Just (ii , ip)
        | Just range <- interactionRange delta ip
        , Just (innerRange, contents) <- virtualFile >>= flip interactionContents range
        -> cont run ii ip range innerRange contents

      _ -> pure . Left $
        ResponseError (InL LSPErrorCodes_RequestFailed) "Cannot find interaction at this point" Nothing

removeHighlightingFor :: (Agda.HasRange a, MonadTCState m) => a -> m ()
removeHighlightingFor x = modifyTCLens' stSyntaxInfo \map -> snd (insideAndOutside (rangeToRange (getRange x)) map)

executeAgdaCommand :: Handlers WorkerM
executeAgdaCommand = requestHandler SMethod_WorkspaceExecuteCommand \req res ->
  case parseCommand (req ^. params . command) (fromMaybe [] (req ^. params . arguments)) of
    Nothing -> res (Left (ResponseError (InL LSPErrorCodes_RequestFailed) "Cannot parse command" Nothing))
    Just (Command_Auto uri pos) -> commandHandlerInteractionPoint uri pos res \run ii ip range innerRange contents -> do
      -- TODO: What's the proper range here? This gets passed from the elisp side, so
      -- I think it's the *inside* of the interaction point, after edits have been applied.
      result <- Mimer.mimer ii (adjustIpRange ip innerRange) (Text.unpack contents)
      case result of
        Mimer.MimerNoResult -> showMessage run MessageType_Error "No solution found"
        MimerExpr str -> do
          let edit = ApplyWorkspaceEditParams (Just "Proof Search") $ WorkspaceEdit
                { _changes = Just (Map.singleton uri [TextEdit range (Text.pack str)])
                , _documentChanges = Nothing
                , _changeAnnotations = Nothing
                }
          void . liftIO . run $ sendRequest SMethod_WorkspaceApplyEdit edit (\_ -> pure ())
        MimerList sols -> showMessage run MessageType_Info . Text.pack $
            "Multiple solutions:" ++
            intercalate "\n" ["  " ++ show i ++ ". " ++ s | (i, s) <- sols]
        MimerClauses{} -> __IMPOSSIBLE__    -- Mimer can't do case splitting yet

      pure (Right (InR Lsp.Null))

    Just (Command_Give uri pos) -> commandHandlerInteractionPoint uri pos res \run ii ip range innerRange contents -> do
      scope <- getInteractionScope ii

      (given, res) <- liftTCM do
        removeHighlightingFor (ipRange ip)

        given <- B.parseExprIn ii (adjustIpRange ip innerRange) (Text.unpack contents)
        res <- B.give WithoutForce ii Nothing given

        highlightExpr res

        reportSLn "lsp.give" 10 $ unlines $
          [ show (toLsp (ipRange ip))
          , show (toLsp (adjustIpRange ip innerRange))
          ]

        pure (given, res)

      ce <- abstractToConcreteScope scope res

      let text = if given == res then contents else Text.pack (prettyShow ce)
      void $ applyEdit run "Give" (Map.singleton uri [TextEdit range text])
      void $ sendRequest SMethod_WorkspaceSemanticTokensRefresh Nothing (const (pure ()))

      delta <- getDelta
      iis <- useTC stInteractionPoints
      forM_ (BiMap.toList iis) \(id, ip) -> do
        reportSLn "lsp.give" 10 $ unlines
          [ "point " <> show id
          , show (toLsp (ipRange ip))
          , show (updatePosition delta (toLsp (ipRange ip)))
          ]

      -- TODO: How do we handle updating interaction points and highlighting here,
      -- short of just rechecking the entire file?

      pure (Right (InR Lsp.Null))

  where
    showMessage run kind msg = liftIO . run $ sendNotification SMethod_WindowShowMessage $ ShowMessageParams kind msg

    applyEdit run title changes =
      let
        edit = ApplyWorkspaceEditParams (Just title) $ WorkspaceEdit
          { _changes = Just changes
          , _documentChanges = Nothing
          , _changeAnnotations = Nothing
          }
      in sendRequest SMethod_WorkspaceApplyEdit edit (\_ -> pure ())

    adjustIpRange ip (Lsp.Range s e) =
      let range = ipRange ip in
      case rangeToInterval range of
        Nothing -> NoRange
        Just (Interval iS iE) ->
          -- TODO: Move this somewhere which doesn't make me cry.
          let
            iS' = iS { posLine = fromIntegral ((s ^. line) + 1), posCol = fromIntegral (s ^. character + 1) }
            iE' = iE { posLine = fromIntegral ((e ^. line) + 1), posCol = fromIntegral (e ^. character + 1) }
          in intervalToRange (rangeFile range) (Interval iS' iE')

lspHandlers :: ClientCapabilities -> Handlers WorkerM
lspHandlers caps = mconcat
  [ onTextDocumentOpen
  , onTextDocumentSaved
  , onTextDocumentClosed
  , onInitialized
  , onTextDocumentChange
  , notificationHandler SMethod_WorkspaceDidChangeConfiguration (const (pure ()))
  , provideSemanticTokens
  , goal
  , Agda.LSP.Main.completion
  , Agda.LSP.Main.onTypeFormatting
  , goToDefinition caps
  , highlightReferences
  , getCodeActions
  , executeAgdaCommand
  ]
