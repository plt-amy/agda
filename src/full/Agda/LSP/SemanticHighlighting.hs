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

import Agda.LSP.Position

import qualified Agda.Utils.RangeMap as RangeMap
import Agda.Utils.RangeMap (RangeMap)
import Agda.Utils.Lens

data ClientTokenLegend = ClientTokenLegend
  { clientTokenTypes :: Set.Set Text.Text
  , clientTokenModifiers :: Set.Set Text.Text
  }
  deriving (Show)

createClientTokens :: SemanticTokensLegend -> ClientTokenLegend
createClientTokens l = ClientTokenLegend
  { clientTokenTypes = Set.fromList (l ^. tokenTypes)
  , clientTokenModifiers = Set.fromList (l ^. tokenModifiers)
  }

aspectToSemantic :: ClientTokenLegend -> Aspect -> Maybe (SemanticTokenTypes, [SemanticTokenModifiers])
aspectToSemantic legend = \case
    Comment -> Just (SemanticTokenTypes_Comment, [])
    Keyword -> Just (SemanticTokenTypes_Keyword, [])
    String  -> Just (SemanticTokenTypes_String, [])
    Number  -> Just (SemanticTokenTypes_Number, [])
    Hole    -> tryType "interactionPoint"
    Symbol  -> tryType "symbol"
    PrimitiveType -> tryType "primitiveType" <|> Just (SemanticTokenTypes_Type, [SemanticTokenModifiers_DefaultLibrary])
    Name Nothing _ -> Just (SemanticTokenTypes_Variable, [])
    Name (Just kind) _ -> case kind of
      Asp.Bound                   -> Just (SemanticTokenTypes_Variable, [])
      Asp.Generalizable           -> Just (SemanticTokenTypes_TypeParameter, [])
      Asp.Constructor Inductive   -> Just (SemanticTokenTypes_EnumMember, [])
      Asp.Constructor CoInductive -> tryType "coinductiveCons" <|> Just (SemanticTokenTypes_EnumMember, [])
      Asp.Datatype                -> Just (SemanticTokenTypes_Enum, [])
      Asp.Field                   -> Just (SemanticTokenTypes_Property, [])
      Asp.Function                -> Just (SemanticTokenTypes_Function, [])
      Asp.Module                  -> Just (SemanticTokenTypes_Namespace, [])
      Asp.Postulate               -> tryType "postulate"
      Asp.Primitive               -> tryType "primitive"
      Asp.Record                  -> Just (SemanticTokenTypes_Struct, [])
      Asp.Argument                -> Just (SemanticTokenTypes_Parameter, [])
      Asp.Macro                   -> Just (SemanticTokenTypes_Macro, [])
    Pragma     -> tryType "pragma" <|> Just (SemanticTokenTypes_Macro, [])
    Background -> Nothing
    Markup     -> Nothing

    where
      tryType :: Text.Text -> Maybe (SemanticTokenTypes, [SemanticTokenModifiers])
      tryType ty
        | ty `Set.member` (clientTokenTypes legend) = Just (SemanticTokenTypes_Custom ty, [])
        | otherwise = Nothing

-- | Convert an aspect map to a list of semantic tokens.
aspectMapToTokens :: ClientCapabilities -> PosDelta -> RangeMap Aspects -> [Lsp.SemanticTokenAbsolute]
aspectMapToTokens caps delta = mapMaybe go . RangeMap.toList where
  tokenCaps = (caps ^. textDocument) >>= view semanticTokens

  -- If augmentsSyntaxTokens is set to False, then we always need to send
  always = maybe False not $ tokenCaps >>= view augmentsSyntaxTokens

  -- Compute the legend sent by the server.
  legend = createClientTokens case tokenCaps of
    Nothing -> defaultSemanticTokensLegend
    Just l -> SemanticTokensLegend { _tokenTypes = l ^. tokenTypes , _tokenModifiers = l ^. tokenModifiers }

  go (range, aspect -> Just asp)
    | always || isInteresting asp
    , Just (ty, mods) <- aspectToSemantic legend asp
    , Just range <- toUpdatedPosition delta range
    , (range ^. Lsp.start . Lsp.line) == (range ^. Lsp.end . Lsp.line)
    = Just Lsp.SemanticTokenAbsolute
        { _tokenType      = ty
        , _line           = range ^. Lsp.start . Lsp.line
        , _startChar      = range ^. Lsp.start . Lsp.character
        , _length         = range ^. Lsp.end . Lsp.character - range ^. Lsp.start . Lsp.character
        , _tokenModifiers = mods
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

-- | The full token legend used by Agda
agdaTokenLegend :: Lsp.SemanticTokensLegend
agdaTokenLegend = Lsp.SemanticTokensLegend
  { _tokenTypes = defaultSemanticTokensLegend ^. tokenTypes ++
      [ "interactionPoint", "symbol", "primitiveType"
      , "coinductiveCons", "postulate", "pragma"
      ]
  , _tokenModifiers = defaultSemanticTokensLegend ^. tokenModifiers
  }

