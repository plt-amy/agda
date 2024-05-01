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
import Agda.LSP.Position (PosDelta, updatePosition)
import Control.Monad (guard)

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
    NoRange -> Lsp.Range (Lsp.Position 0 0) (Lsp.Position 0 0) -- TODO

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
    Hole    -> Lsp.SemanticTokenTypes_Custom "interactionPoint"
    Symbol  -> Lsp.SemanticTokenTypes_Custom "symbol"
    PrimitiveType -> Lsp.SemanticTokenTypes_Custom "primitiveType"
    Name Nothing _ -> Lsp.SemanticTokenTypes_Variable
    Name (Just kind) _ -> case kind of
      Asp.Bound                   -> Lsp.SemanticTokenTypes_Variable
      Asp.Generalizable           -> Lsp.SemanticTokenTypes_TypeParameter
      Asp.Constructor Inductive   -> Lsp.SemanticTokenTypes_EnumMember
      Asp.Constructor CoInductive -> Lsp.SemanticTokenTypes_Custom "coinductiveCons"
      Asp.Datatype                -> Lsp.SemanticTokenTypes_Enum
      Asp.Field                   -> Lsp.SemanticTokenTypes_Property
      Asp.Function                -> Lsp.SemanticTokenTypes_Function
      Asp.Module                  -> Lsp.SemanticTokenTypes_Namespace
      Asp.Postulate               -> Lsp.SemanticTokenTypes_Custom "postulate"
      Asp.Primitive               -> Lsp.SemanticTokenTypes_Custom "primitive"
      Asp.Record                  -> Lsp.SemanticTokenTypes_Struct
      Asp.Argument                -> Lsp.SemanticTokenTypes_Parameter
      Asp.Macro                   -> Lsp.SemanticTokenTypes_Macro
    Pragma     -> Lsp.SemanticTokenTypes_Custom "pragma"
    Background -> Lsp.SemanticTokenTypes_Custom "background"
    Markup     -> Lsp.SemanticTokenTypes_Custom "markup"

agdaTokenLegend :: Lsp.SemanticTokensLegend
agdaTokenLegend = Lsp.SemanticTokensLegend
  { _tokenTypes =
    [ "comment", "keyword", "string", "number", "interactionPoint",
    "symbol", "primitiveType", "variable", "typeParameter",
    "enumMember", "coinductiveCons", "enum", "property", "function",
    "namespace", "postulate", "primitive", "struct", "parameter",
    "macro", "pragma", "background", "markup" ]
  , _tokenModifiers = []
  }

aspectMapToTokens :: PosDelta -> RangeMap Aspects -> [Lsp.SemanticTokenAbsolute]
aspectMapToTokens delta = concatMap go . RangeMap.toList where
  go (_, asp@Aspects{aspectRange = range}) | range /= noRange = case aspect asp of
    Just Symbol -> []
    Just asp ->
      let
        tok ival = do
          Lsp.Position line col <- updatePosition delta (toLsp (iStart ival))
          Lsp.Position line' col' <- updatePosition delta (toLsp (iEnd ival))
          guard (line == line')

          pure Lsp.SemanticTokenAbsolute
            { _tokenType      = toLsp asp
            , _line           = line
            , _startChar      = col
            , _length         = col' - col
            , _tokenModifiers = []
            }
      in mapMaybe tok (rangeIntervals range)
      -- Just OffsetToken
      -- { tokenStart     = start
      -- , tokenEnd       = end
      -- , tokenType      = toLsp asp
      -- , tokenSyntactic = tokenBased aspects == TokenBased
      -- }
    _ -> []
  go _ = []
