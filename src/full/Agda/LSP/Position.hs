{-# LANGUAGE OverloadedLabels #-}
module Agda.LSP.Position where

import Control.Monad

import qualified Data.Text as T
import Data.Row.Records

import Language.LSP.Protocol.Types as Lsp
import Language.LSP.Protocol.Lens

import Agda.Utils.Lens

data Result a
  = RangeR a a
  | ExactR a
  deriving (Eq, Show, Ord, Functor)

prLower :: Result a -> a
prLower (RangeR x _) = x
prLower (ExactR x) = x

prUpper :: Result a -> a
prUpper (RangeR x _) = x
prUpper (ExactR x) = x

instance Applicative Result where
  pure = ExactR

  ExactR f <*> a = fmap f a
  RangeR f g <*> ExactR x = RangeR (f x) (g x)
  RangeR f g <*> RangeR x y = RangeR (f x) (g y)

instance Monad Result where
  ExactR x >>= f = f x
  RangeR x y >>= f = RangeR (prLower (f x)) (prUpper (f y))

data PosDelta = PosDelta
  { toDelta   :: !(Position -> Result Position)
  , fromDelta :: !(Position -> Result Position)
  }

instance Semigroup PosDelta where
  PosDelta f1 g1 <> PosDelta f2 g2 = PosDelta (f1 <=< f2) (g2 >=> g1)

instance Monoid PosDelta where
  mempty = PosDelta pure pure

toCurrent :: Range -> T.Text -> Position -> Result Position
toCurrent (Range start@(Position startLine startColumn) end@(Position endLine endColumn)) t (Position line column)
  | line < startLine || line == startLine && column < startColumn =
    -- Position is before the change and thereby unchanged.
    ExactR $ Position line column
  | line > endLine || line == endLine && column >= endColumn =
    -- Position is after the change so increase line and column number
    -- as necessary.
    ExactR $ newLine `seq` newColumn `seq` Position newLine newColumn
  | otherwise = RangeR start end
  -- Position is in the region that was changed.
  where
    lineDiff = linesNew - linesOld
    linesNew = T.count "\n" t
    linesOld = fromIntegral endLine - fromIntegral startLine

    newEndColumn :: UInt
    newEndColumn
      | linesNew == 0 = fromIntegral $ fromIntegral startColumn + T.length t
      | otherwise = fromIntegral $ T.length $ T.takeWhileEnd (/= '\n') t

    newColumn :: UInt
    newColumn
      | line == endLine = (column + newEndColumn) - endColumn
      | otherwise = column

    newLine :: UInt
    newLine = fromIntegral $ fromIntegral line + lineDiff

fromCurrent :: Range -> T.Text -> Position -> Result Position
fromCurrent (Range start@(Position startLine startColumn) end@(Position endLine endColumn)) t (Position line column)
  | line < startLine || line == startLine && column < startColumn =
    -- Position is before the change and thereby unchanged
    ExactR $ Position line column
  | line > newEndLine || line == newEndLine && column >= newEndColumn =
    -- Position is after the change so increase line and column number
    -- as necessary.
    ExactR $ newLine `seq` newColumn `seq` Position newLine newColumn
  | otherwise = RangeR start end
  -- Position is in the region that was changed.
  where
    lineDiff = linesNew - linesOld
    linesNew = T.count "\n" t
    linesOld = fromIntegral endLine - fromIntegral startLine

    newEndLine :: UInt
    newEndLine = fromIntegral $ fromIntegral endLine + lineDiff

    newEndColumn :: UInt
    newEndColumn
      | linesNew == 0 = fromIntegral $ fromIntegral startColumn + T.length t
      | otherwise = fromIntegral $ T.length $ T.takeWhileEnd (/= '\n') t

    newColumn :: UInt
    newColumn
      | line == newEndLine = (column + endColumn) - newEndColumn
      | otherwise = column

    newLine :: UInt
    newLine = fromIntegral $ fromIntegral line - lineDiff

changeToDelta :: TextDocumentContentChangeEvent -> PosDelta
changeToDelta (TextDocumentContentChangeEvent (InL x)) =
  PosDelta (toCurrent (x .! #range) (x .! #text)) (fromCurrent (x .! #range) (x .! #text))
changeToDelta _ = mempty


class Positionable a where
  updatePosition :: PosDelta -> a -> Maybe a
  downgradePosition :: PosDelta -> a -> Maybe a


instance Positionable Position where
  updatePosition delta pos = case toDelta delta pos of
    ExactR r -> Just r
    _ -> Nothing

  downgradePosition delta pos = case fromDelta delta pos of
    ExactR r -> Just r
    _ -> Nothing

instance Positionable Range where
  updatePosition delta (Range s e) = Range <$> updatePosition delta s <*> updatePosition delta e
  downgradePosition delta (Range s e) = Range <$> downgradePosition delta s <*> downgradePosition delta e
