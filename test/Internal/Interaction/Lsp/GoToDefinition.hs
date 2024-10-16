module Internal.Interaction.Lsp.GoToDefinition (tests) where

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

gotoDefinition :: TestTree
gotoDefinition = testCase "Give adds parentheses" $ runAgdaSession def lspRoot $ do
  doc <- openDoc "Basic.agda" "agda"
  let uriDoc = doc ^. Lsp.uri
  waitForSuccessfulReload

  let unitDef = InL . Definition . InL $ Location uriDoc (Range (Position 2 5) (Position 2 9))

  -- Get definition of "Unit" at the left most character.
  defs <- getDefinitions doc (Position 5 4)
  liftIO $ defs @?= unitDef

  -- Get definition of "Unit" at the right most character.
  defs <- getDefinitions doc (Position 5 8)
  liftIO $ defs @?= unitDef

  pure ()

tests :: TestTree
tests = testGroup "Internal.Interaction.Lsp.GoToDefinition"
  [ gotoDefinition
  ]
