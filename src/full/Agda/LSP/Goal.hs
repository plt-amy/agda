{-# LANGUAGE DataKinds, DisambiguateRecordFields #-}
module Agda.LSP.Goal where

import qualified Language.LSP.Protocol.Types as Lsp
import Language.LSP.Protocol.Types

import GHC.Generics

import qualified Agda.Syntax.Concrete as C
import qualified Agda.Syntax.Abstract as A
import Agda.Syntax.Concrete.Pretty ()
import Agda.Syntax.Common.Pretty
import Agda.Syntax.Internal
import Agda.Syntax.Common

import Agda.TypeChecking.Monad

import Agda.Interaction.BasicOps ()
import Agda.Interaction.Base (Rewrite)

import Agda.TypeChecking.Pretty ()
import Agda.Interaction.JSON

import Agda.LSP.Monad.Base
import Agda.LSP.Output

data Query a where
  Query_GoalAt   :: Lsp.Position  -> Query (Maybe InteractionId)
  Query_GoalInfo :: InteractionId -> Query GoalInfo
  Query_AllGoals :: Query [Goal]

deriving instance Show (Query a)
deriving newtype instance FromJSON InteractionId
deriving newtype instance ToJSON InteractionId

data SomeQuery where
  SomeQuery :: forall a. ToJSON a => Lsp.Uri -> Query a -> SomeQuery

deriving instance Show SomeQuery

data Goal = Goal
  { goalId       :: InteractionId
  , goalRange    :: Range
  , goalType     :: Printed C.Expr
  }
  deriving (Show, Generic)

instance ToJSON Goal

data ReifiedName = ReifiedName
  { reifiedNameActual :: C.Name
  , reifiedName       :: C.Name
  }
  deriving (Show, Generic)

instance Pretty ReifiedName where
  pretty (ReifiedName n x)
    | n == x                     = pretty x
    | C.isInScope n == C.InScope = pretty n
    | otherwise                  = pretty x

data Binder = Binder
  { binderName    :: ReifiedName
  , binderType    :: C.Expr
  , binderIsValue :: Bool
  }
  deriving (Show, Generic)

instance Pretty Binder where
  pretty (Binder name expr islet) = sep [ pretty name <+> sym, nest 2 (pretty expr) ] where
    sym | islet     = equals
        | otherwise = colon

data Local = Local
  { localBinder      :: Printed Binder
  , localBindingSite :: Maybe Lsp.Range
  , localValue       :: Maybe (Printed Binder)
  , localInScope     :: Bool
  , localHiding      :: Hiding
  , localModality    :: Modality
  }
  deriving (Show, Generic)

instance ToJSON Relevance where
  toJSON = toJSON . show

instance ToJSON Quantity where
  toJSON = \case
    Quantity0{} -> "0"
    Quantity1{} -> "1"
    Quantityω{} -> "ω"

instance ToJSON Cohesion where
  toJSON = toJSON . show

instance ToJSON Modality

instance ToJSON Hiding where
  toJSON = \case
    Hidden      -> "Hidden"
    Instance{}  -> "Instance"
    NotHidden{} -> "NotHidden"

instance ToJSON Local

data GoalInfo = GoalInfo
  { goalGoal     :: Goal
  , goalContext  :: [Local]
  , goalBoundary :: Maybe [Printed (IPFace' C.Expr)]
  }
  deriving (Show, Generic)

instance ToJSON GoalInfo

instance FromJSON SomeQuery where
  parseJSON = withObject "Query" \obj -> do
    uri <- obj .: "uri"
    obj .: "kind" >>= withText "kind" \case
      "GoalAt"   -> SomeQuery uri <$> Query_GoalAt <$> obj .: "position"
      "GoalInfo" -> SomeQuery uri <$> (Query_GoalInfo <$> obj .: "goal")
      "AllGoals" -> pure $ SomeQuery uri Query_AllGoals
      _ -> fail "Unknown query kind"

instance ToJSON SomeQuery where
  toJSON (SomeQuery uri query) =
    let
      kind :: String
      (kind, rest) = case query of
        Query_GoalAt pos -> ("GoalAt", ["position" .= toJSON pos])
        Query_GoalInfo iid -> ("GoalInfo", ["goal" .= toJSON iid])
        Query_AllGoals -> ("AllGoals", [])
    in
    object ("uri" .= toJSON uri : "kind" .= toJSON kind : rest)

-- data GoalResponse a
--   = AllGoals
--     { goals :: [Goal a] }
--   | OneGoal
--     { goalGoal    :: Goal a
--     , goalContext :: [Local a]
--     , goalFaces   :: [TC.IPFace' a]
--     }

-- data Judgement a = OfType a | IsSort

-- data Goal a = Goal
--   { goalMeta      :: InteractionId
--   , goalRange     :: Range
--   , goalJudgement :: Judgement a
--   }

-- data Local a = Local
--   { localName        :: C.Name
--   , localReifiedName :: C.Name
--   , localType        :: a
--   , localLet         :: Maybe a
--   , localInScope     :: C.NameInScope
--   }
