module Tiger.Util.SourcePos where

import Data.Text (Text)

type SourcePath = Text

data SourceLocation = SourceLocation
  { srcLocOffset :: !Int
  , srcLocRow :: !Int
  , srcLocColumn :: !Int
  }
  deriving (Eq, Ord, Show)

data Span = Span
  { spanStart :: SourceLocation
  , spanEnd :: SourceLocation
  }
  deriving (Eq, Ord, Show)

data SourceRegion = SourceRegion
  { filePath :: SourcePath
  , span :: Span
  }
  deriving (Eq, Ord, Show)

mergeSpans :: [Span] -> Span
mergeSpans = undefined

addHorizontalOffset :: Int -> SourceLocation -> SourceLocation
addHorizontalOffset l src = src{srcLocOffset = srcLocOffset src + l, srcLocColumn = srcLocColumn src + l}

makeSpanOfLength :: Int -> SourceLocation -> Span
makeSpanOfLength l src = Span src $ addHorizontalOffset l src

makeSpanFromString :: String -> SourceLocation -> Span
makeSpanFromString s start = makeSpanFromString' s start
 where
  makeSpanFromString' [] end = Span start end
  makeSpanFromString' (x : xs) (SourceLocation o r c) = case x of
    '\n' -> makeSpanFromString' xs $ SourceLocation (o + 1) (r + 1) 1
    '\t' -> makeSpanFromString' xs $ SourceLocation (o + 1) r (c + 8)
    _ -> makeSpanFromString' xs $ SourceLocation (o + 1) r (c + 1)