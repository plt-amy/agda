module Agda.LSP.Translation where

import qualified Data.Text as Text
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Aeson (ToJSON)

import Control.Monad (guard)

import qualified Language.LSP.Protocol.Types as Lsp
import qualified Language.LSP.Protocol.Lens as Lsp

import Agda.Syntax.Common.Pretty
import Agda.Syntax.Position

import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Errors ()

import qualified Agda.Interaction.Highlighting.Range as Hl
import qualified Agda.Utils.RangeMap as RangeMap
import Agda.Utils.RangeMap (RangeMap)
import Agda.Utils.Impossible
import Agda.Utils.Lens
import Agda.Syntax.Common.Aspect as Asp
import qualified Agda.Utils.Maybe.Strict as Strict
import Agda.Utils.FileName (filePath)
import Agda.Syntax.Abstract.Name (Name(nameBindingSite), qnameName)
import Agda.LSP.Position

class ToLsp a where
  type LspType a
  toLsp :: a -> LspType a

instance ToLsp (Position' a) where
  type LspType (Position' a) = Lsp.Position
  toLsp (Pn _file _boff line col) = Lsp.Position (fromIntegral (line - 1)) (fromIntegral (col - 1))

instance ToLsp (Interval' a) where
  type LspType (Interval' a) = Lsp.Range
  toLsp (Interval start end) =
    let
      Lsp.Position le ce = toLsp end
    in Lsp.Range (toLsp start) (Lsp.Position le ce)

instance ToLsp (Range' a) where
  type LspType (Range' a) = Lsp.Range
  toLsp range = case rangeToIntervalWithFile range of
    Nothing -> Lsp.Range (Lsp.Position 0 0) (Lsp.Position 0 0) -- TODO
    -- TODO: Agda has non-contiguous ranges but LSP just wants a start
    -- and end position.
    Just i -> toLsp (Interval (iStart i) (iEnd i))
