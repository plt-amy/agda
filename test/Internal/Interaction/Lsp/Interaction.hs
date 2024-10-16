module Internal.Interaction.Lsp.Interaction (tests) where

import qualified Language.LSP.Protocol.Lens as Lsp
import Language.LSP.Protocol.Types
import Language.LSP.Test

import Control.Monad.IO.Class

import qualified Data.Text as Text
import Data.Default

import Test.Tasty.HUnit

import Internal.Helpers
import Internal.Interaction.Lsp.Utils

import Agda.LSP.Commands
import Agda.LSP.Goal
import Agda.Utils.Lens

giveAddsParentheses :: TestTree
giveAddsParentheses = testCase "Give adds parentheses" $ runAgdaSession def lspRoot $ do
  doc <- openDoc "Give.agda" "agda"
  let uriDoc = doc ^. Lsp.uri
  waitForSuccessfulReload

  insertText doc (Position 6 23) "add {!!} {!!} "

  [Goal {goalRange = Range (Position 6 20) (Position 6 39)}] <- goalQuery doc $ Query_AllGoals False
  res <- executeCommand (toCommand "Give" (Command_Give uriDoc (Position 6 20)))

  newContents <- getDocumentEdit doc
  liftIO $ Text.lines newContents !! 6 @?= "add (suc x) y = suc (add {!!} {!!})"

  ( [ Goal {goalRange = Range (Position 6 25) (Position 6 29)}
    , Goal {goalRange = Range (Position 6 30) (Position 6 34)} ] ) <- goalQuery doc $ Query_AllGoals False

  pure ()

giveReplacesHoles :: TestTree
giveReplacesHoles = testCase "Give replaces holes" $ runAgdaSession def lspRoot $ do
  doc <- openDoc "Give.agda" "agda"
  let uriDoc = doc ^. Lsp.uri
  waitForSuccessfulReload

  insertText doc (Position 6 23) "add ? (suc ?) "

  [Goal {goalRange = Range (Position 6 20) (Position 6 39)}] <- goalQuery doc $ Query_AllGoals False
  res <- executeCommand (toCommand "Give" (Command_Give uriDoc (Position 6 20)))

  newContents <- getDocumentEdit doc
  liftIO $ Text.lines newContents !! 6 @?= "add (suc x) y = suc (add {! !} (suc {! !}))"

  ( [ Goal {goalRange = Range (Position 6 25) (Position 6 30)}
    , Goal {goalRange = Range (Position 6 36) (Position 6 41)} ] ) <- goalQuery doc $ Query_AllGoals False

  tokens <- getAbsSemanticTokens doc
  let
    token = SemanticTokenAbsolute
      { _line = 6
      , _startChar = 32
      , _length = 3
      , _tokenType = SemanticTokenTypes_EnumMember
      , _tokenModifiers = []
      }
  liftIO $ elem token tokens @? "Expected updated [" ++ concat (map (\x -> "\n  " ++ show x) tokens) ++ " ] to contain " ++ show token

  -- Save the file and assert they are the same.
  saveFile doc
  waitForSuccessfulReload

  tokens' <- getAbsSemanticTokens doc
  liftIO $ tokens' @?= tokens

  pure ()

tests :: TestTree
tests = testGroup "Internal.Interaction.Lsp.Interaction"
  [ giveAddsParentheses
  , giveReplacesHoles
  ]
