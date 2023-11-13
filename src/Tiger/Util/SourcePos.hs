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
addHorizontalOffset l src = src{srcLocOffset = srcLocOffset src + l - 1, srcLocColumn = srcLocColumn src + l - 1}

makeSpanOfLength :: Int -> SourceLocation -> Span
makeSpanOfLength l src = Span src $ addHorizontalOffset l src