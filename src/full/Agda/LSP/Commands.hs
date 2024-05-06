{-# LANGUAGE DataKinds #-}
module Agda.LSP.Commands
  ( AgdaCommand(..)
  , commandName
  , CommandArgs(..)
  , toCommand
  , parseCommand
  ) where

import Data.Text (Text)
import Data.Aeson (Value, Result(..), toJSON, fromJSON, ToJSON, FromJSON)
import qualified Data.Text as Text

import Language.LSP.Protocol.Types as Lsp
import Agda.Compiler.Backend (primAgdaTCMCommit)
import GHC.Generics (Generic)


-- | The possible commands the LSP server supports
data AgdaCommand
  = Auto
  | Give
  deriving (Show, Enum, Bounded)

-- | Get the name of the command that used when converting to a command
commandName :: AgdaCommand -> Text
commandName Auto = "auto"
commandName Give = "give"

data CommandArgs
  -- | Run Mimer on the interaction point at the given range.
  = Command_Auto Uri Position
  | Command_Give Uri Position
  deriving (Generic)

toCommand :: Text -> CommandArgs -> Command
toCommand title (Command_Auto uri pos) = Command title (commandName Auto) . Just $
  [ toJSON uri
  , toJSON pos
  ]
toCommand title (Command_Give uri pos) = Command title (commandName Give) . Just $
  [ toJSON uri
  , toJSON pos
  ]

parseCommand :: Text -> [Value] -> Maybe CommandArgs
parseCommand command args
  | command == commandName Auto, [uri, pos] <- args = Command_Auto <$> parse uri <*> parse pos
  | command == commandName Give, [uri, pos] <- args = Command_Give <$> parse uri <*> parse pos

  | otherwise = Nothing

  where
    parse :: FromJSON a => Value -> Maybe a
    parse = resToMaybe . fromJSON

    resToMaybe (Success x) = Just x
    resToMaybe (Error _) = Nothing
