{-# LANGUAGE DataKinds, DisambiguateRecordFields #-}
module Agda.LSP.Goal where

import qualified Language.LSP.Protocol.Types as Lsp
import Language.LSP.Protocol.Types

import Data.Aeson.Types
import Data.Function
import Data.Aeson

import GHC.Generics

import qualified Agda.Syntax.Concrete as C
import qualified Agda.Syntax.Abstract as A
import Agda.Syntax.Common.Pretty (Pretty)
import Agda.Syntax.Internal
import Agda.Syntax.Common

import Agda.TypeChecking.Monad

import Agda.Interaction.Base (Rewrite)
import Agda.TypeChecking.Pretty
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
  { goalId    :: InteractionId
  , goalRange :: Range
  , goalType  :: Printed C.Expr
  }
  deriving (Generic)

instance ToJSON Goal

data Local = Local
  { localName        :: Printed C.Name
  , localReifiedName :: Printed C.Name
  , localType        :: Printed C.Expr
  }
  deriving (Generic)

instance ToJSON Local

data GoalInfo = GoalInfo
  { goalGoal    :: Goal
  , goalContext :: [Local]
  }
  deriving (Generic)

instance ToJSON GoalInfo

instance FromJSON SomeQuery where
  parseJSON = withObject "Query" \obj -> do
    uri <- obj .: "uri"
    obj .: "kind" >>= withText "kind" \case
      "GoalAt"   -> SomeQuery uri <$> Query_GoalAt <$> obj .: "position"
      "GoalInfo" -> SomeQuery uri <$> (Query_GoalInfo <$> obj .: "goal")
      "AllGoals" -> pure $ SomeQuery uri Query_AllGoals
      _ -> fail "Unknown query kind"

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
