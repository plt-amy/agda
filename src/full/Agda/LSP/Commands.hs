{-# LANGUAGE DataKinds #-}
module Agda.LSP.Commands
  ( AgdaCommand(..)
  , commandName
  , CommandArgs(..)
  , toCommand
  , parseCommand
  ) where

import Data.Text (Text)
import Data.Aeson (Value, Result(Success), toJSON, fromJSON)
import qualified Data.Text as Text

import Language.LSP.Protocol.Types as Lsp
import Agda.Compiler.Backend (primAgdaTCMCommit)


-- | The possible commands the LSP server supports
data AgdaCommand
  = Auto
  deriving (Show, Enum, Bounded)

-- | Get the name of the command that used when converting to a command
commandName :: AgdaCommand -> Text
commandName Auto = "auto"

data CommandArgs where
  -- | Run Mimer on the interaction point at the given range.
  Command_Auto :: Uri -> Position -> CommandArgs

toCommand :: Text -> CommandArgs -> Command
toCommand title (Command_Auto uri pos) = Command title (commandName Auto) . Just $
  [ toJSON uri
  , toJSON pos
  ]

parseCommand :: Text -> [Value] -> Maybe CommandArgs
parseCommand command args
  | command == commandName Auto
  , [uri, pos] <- args
  , Success uri' <- fromJSON uri
  , Success pos' <- fromJSON pos
  = Just (Command_Auto uri' pos')
  | otherwise = Nothing
