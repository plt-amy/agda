module Agda.LSP.Translation where

import qualified Data.Text as Text
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Aeson (ToJSON)

import qualified Language.LSP.Protocol.Types as Lsp
import qualified Language.LSP.Protocol.Lens as Lsp

import GHC.Generics

import Agda.Syntax.Common.Pretty
import Agda.Syntax.Position

import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Errors

import qualified Agda.Interaction.Highlighting.Range as Hl
import qualified Agda.Utils.RangeMap as RangeMap
import Agda.Utils.RangeMap (RangeMap)
import Agda.Utils.Impossible
import Agda.Utils.Lens
import Agda.Syntax.Common.Aspect as Asp
import qualified Agda.Utils.Maybe.Strict as Strict
import Agda.Utils.FileName (filePath)
import Agda.Syntax.Abstract.Name (Name(nameBindingSite), qnameName)

class ToLsp a where
  type LspType a
  toLsp :: a -> LspType a

instance ToLsp (Position' a) where
  type LspType (Position' a) = Lsp.Position
  toLsp (Pn _file _boff line col) = Lsp.Position (fromIntegral (line - 1)) (fromIntegral (col - 1))

instance ToLsp (Interval' a) where
  type LspType (Interval' a) = Lsp.Range
  toLsp (Interval start end) = Lsp.Range (toLsp start) (toLsp end)

instance ToLsp (Range' a) where
  type LspType (Range' a) = Lsp.Range
  toLsp = \case
    NoRange -> __IMPOSSIBLE__

    -- TODO: Agda has non-contiguous ranges but LSP just wants a start
    -- and end position.
    Range _ is ->
      let
        is' = toList is
      in toLsp (Interval (iStart (head is')) (iEnd (last is')))

errorToDiagnostic :: TCErr -> TCM [Lsp.Diagnostic]
errorToDiagnostic =
  \case
    err@TypeError{} -> do
      msg <- unlines . tail . lines . render <$> prettyTCM err

      let
        diag = withRangeMsg (getRange err) msg

      diag <- pure case clValue (tcErrClosErr err) of
        ClashingDefinition _ old _ -> diag &
          set Lsp.relatedInformation
            (Just [ Lsp.DiagnosticRelatedInformation
              { _location = Lsp.Location
                  (Lsp.filePathToUri (filePath (rangeFilePath (Strict.fromJust (rangeFile (nameBindingSite (qnameName old)))))))
                  (toLsp (nameBindingSite (qnameName old)))
              , _message  = "Previous definition here"
              } ])
        _ -> diag

      pure [diag]

    Exception r d     -> pure $ [withRangeMsg r $ render d]
    IOException _ r e -> pure $ [withRangeMsg r $ show e]
    PatternErr{} -> __IMPOSSIBLE__
  where
    withRangeMsg r msg = Lsp.Diagnostic
      { _range              = toLsp r
      , _severity           = Just Lsp.DiagnosticSeverity_Error
      , _code               = Nothing
      , _codeDescription    = Nothing
      , _source             = Just "agda"
      , _message            = Text.pack msg
      , _tags               = Nothing
      , _relatedInformation = Nothing
      , _data_              = Nothing
      }

instance ToLsp Aspect where
  type LspType Aspect = Lsp.SemanticTokenTypes
  toLsp = \case
    Comment -> Lsp.SemanticTokenTypes_Comment
    Keyword -> Lsp.SemanticTokenTypes_Keyword
    String  -> Lsp.SemanticTokenTypes_String
    Number  -> Lsp.SemanticTokenTypes_Number
    Hole    -> Lsp.SemanticTokenTypes_Custom "InteractionPoint"
    Symbol  -> Lsp.SemanticTokenTypes_Custom "Symbol"
    PrimitiveType -> Lsp.SemanticTokenTypes_Type
    Name Nothing _ -> Lsp.SemanticTokenTypes_Variable
    Name (Just kind) _ -> case kind of
      Asp.Bound         -> Lsp.SemanticTokenTypes_Variable
      Asp.Generalizable -> Lsp.SemanticTokenTypes_Variable
      Asp.Constructor _ -> Lsp.SemanticTokenTypes_EnumMember
      Asp.Datatype      -> Lsp.SemanticTokenTypes_Enum
      Asp.Field         -> Lsp.SemanticTokenTypes_Property
      Asp.Function      -> Lsp.SemanticTokenTypes_Function
      Asp.Module        -> Lsp.SemanticTokenTypes_Namespace
      Asp.Postulate     -> Lsp.SemanticTokenTypes_Custom "Postulate"
      Asp.Primitive     -> Lsp.SemanticTokenTypes_Custom "Primitive"
      Asp.Record        -> Lsp.SemanticTokenTypes_Struct
      Asp.Argument      -> Lsp.SemanticTokenTypes_Variable
      Asp.Macro         -> Lsp.SemanticTokenTypes_Macro
    Pragma     -> Lsp.SemanticTokenTypes_Custom "Pragma"
    Background -> Lsp.SemanticTokenTypes_Custom "Background"
    Markup     -> Lsp.SemanticTokenTypes_Custom "Markup"

data OffsetToken = OffsetToken
  { tokenStart :: Int
  , tokenEnd   :: Int
  , tokenType  :: Lsp.SemanticTokenTypes
  }
  deriving (Show, Generic)

instance ToJSON OffsetToken

aspectMapToTokens :: RangeMap Aspects -> [OffsetToken]
aspectMapToTokens = mapMaybe go . RangeMap.toList where
  go (Hl.Range start end, aspects) = case aspect aspects of
    Just asp -> Just OffsetToken
      { tokenStart = start
      , tokenEnd   = end
      , tokenType  = toLsp asp
      }
    _ -> Nothing
