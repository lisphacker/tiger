module Tiger.Util.SourcePos where

import Data.Text (Text)

type SourcePath = Text

data SourceLocation = SourceLocation
  { srcLocOffset :: !Int
  , srcLocRow :: !Int
  , srcLocColumn :: !Int
  }
  deriving (Eq, Show)

instance Ord SourceLocation where
  compare (SourceLocation o1 _ _) (SourceLocation o2 _ _) = compare o1 o2

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

uninitializedSourceRegion :: SourceRegion
uninitializedSourceRegion = SourceRegion "" $ Span (SourceLocation (-1) 0 0) (SourceLocation (-1) 0 0)

mergeSpans :: Span -> Span -> Span
mergeSpans (Span (SourceLocation (-1) _ _) _) s = s
mergeSpans (Span _ (SourceLocation (-1) _ _)) s = s
mergeSpans s (Span (SourceLocation (-1) _ _) _) = s
mergeSpans s (Span _ (SourceLocation (-1) _ _)) = s
mergeSpans (Span s1 e1) (Span s2 e2) = Span (min s1 e1) (max e1 e2)

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

class HasSourceRegion a where
  sourceRegion :: a -> SourceRegion

mergeSourceRegions :: SourceRegion -> SourceRegion -> SourceRegion
mergeSourceRegions (SourceRegion fp1 s1) (SourceRegion _ s2) = SourceRegion fp1 $ mergeSpans s1 s2

mergeHasSourceRegions :: (HasSourceRegion a, HasSourceRegion b) => a -> b -> SourceRegion
mergeHasSourceRegions a b = mergeSourceRegions (sourceRegion a) (sourceRegion b)

(<+>) :: (HasSourceRegion a, HasSourceRegion b) => a -> b -> SourceRegion
(<+>) = mergeHasSourceRegions

mergeHasSourceRegionsList :: (HasSourceRegion a) => [a] -> SourceRegion
mergeHasSourceRegionsList as = foldl mergeSourceRegions uninitializedSourceRegion $ map sourceRegion as

mergeHasSourceRegionsWithList :: (HasSourceRegion a, HasSourceRegion b) => a -> [b] -> SourceRegion
mergeHasSourceRegionsWithList a bs = foldl mergeSourceRegions (sourceRegion a) (map sourceRegion bs)
