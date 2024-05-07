module Agda.LSP.Output
  ( Printed(..)
  , renderToJSON
  )
  where

import Language.LSP.Protocol.Types (SemanticTokenTypes)

import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import Data.Aeson.Types

import GHC.Generics

import qualified Text.PrettyPrint.Annotated.HughesPJ as Ppr
import qualified Text.PrettyPrint.Annotated as Ppr
import Agda.Syntax.Common.Pretty
import Agda.Syntax.Common.Aspect ( Aspects (aspect) )
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.LSP.Translation (ToLsp(toLsp))

data DocTree = Node SemanticTokenTypes [DocTree] | Text Text.Text | Mark (Maybe SemanticTokenTypes)
  deriving Generic

instance ToJSON DocTree where
  toJSON = \case
    Node tt ds -> Object $ KeyMap.fromList [ ("tag", toJSON tt), ( "children", toJSONList ds) ]
    Text t -> toJSON t
    Mark t -> toJSON ("mark" :: String)

renderToJSON :: Doc -> Value
renderToJSON = toJSON . Ppr.fullRenderAnn Ppr.LeftMode 100 1.5 cont [] where
  consText (Ppr.Chr c) (Text t:ts) = Text (c `Text.cons` t):ts
  consText (Ppr.Str c) (Text t:ts) = Text (Text.pack c <> t):ts
  consText (Ppr.PStr c) (Text t:ts) = Text (Text.pack c <> t):ts
  consText (Ppr.Chr c) ts = Text (Text.singleton c):ts
  consText (Ppr.Str c) ts = Text (Text.pack c):ts
  consText (Ppr.PStr c) ts = Text (Text.pack c):ts

  annotate acc (Mark (Just t):ts) = Node t (reverse acc):ts
  annotate acc (Mark Nothing:ts) = reverse acc <> ts
  annotate acc (t:ts) = annotate (t:acc) ts
  annotate acc [] = __IMPOSSIBLE__

  cont :: Ppr.AnnotDetails Aspects -> [DocTree] -> [DocTree]
  cont ann acc = case ann of
    Ppr.AnnotStart  -> annotate [] acc
    Ppr.NoAnnot d _ -> consText d acc
    Ppr.AnnotEnd a
      | Just asp <- aspect a -> Mark (Just (toLsp asp)):acc
      | otherwise -> Mark Nothing:acc -- uncurry (<>) (break acc)

newtype Printed a = Printed { getPrinted :: a }

instance Pretty a => ToJSON (Printed a) where
  toJSON = renderToJSON . pretty . getPrinted
