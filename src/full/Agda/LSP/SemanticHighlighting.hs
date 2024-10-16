module Agda.LSP.SemanticHighlighting
  ( aspectMapToTokens
  , agdaTokenLegend
  ) where

import Control.Applicative

import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)

import Language.LSP.Protocol.Types as Lsp
import Language.LSP.Protocol.Lens as Lsp

import Agda.Syntax.Common.Aspect as Asp

import Agda.LSP.Translation
import Agda.LSP.Position

import qualified Agda.Utils.RangeMap as RangeMap
import Agda.Utils.RangeMap (RangeMap)
import Agda.Utils.Lens


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

-- | The token legend containing our custom semantic tokens.
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

-- | Convert an aspect map to a list of semantic tokens.
aspectMapToTokens :: PosDelta -> RangeMap Aspects -> [Lsp.SemanticTokenAbsolute]
aspectMapToTokens delta = mapMaybe go . RangeMap.toList where
  go (range, aspect -> Just asp)
    | isInteresting asp
    , Just range <- toUpdatedPosition delta range
    , (range ^. Lsp.start . Lsp.line) == (range ^. Lsp.end . Lsp.line)
    = Just Lsp.SemanticTokenAbsolute
        { _tokenType      = toLsp asp
        , _line           = range ^. Lsp.start . Lsp.line
        , _startChar      = range ^. Lsp.start . Lsp.character
        , _length         = range ^. Lsp.end . Lsp.character - range ^. Lsp.start . Lsp.character
        , _tokenModifiers = []
        }
  go _ = Nothing

  isInteresting :: Aspect -> Bool
  isInteresting Comment       = False
  isInteresting Keyword       = False
  isInteresting String        = False
  isInteresting Number        = False
  isInteresting Hole          = True
  isInteresting Symbol        = False
  isInteresting PrimitiveType = True
  isInteresting Name{}        = True
  isInteresting Pragma        = False
  isInteresting Background    = False
  isInteresting Markup        = False
