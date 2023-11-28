module Tiger.Util.SourcePos where

data SourceLocation = SourceLocation
  { srcLocOffset :: !Int
  , srcLocRow :: !Int
  , srcLocColumn :: !Int
  }
  deriving (Eq, Show)

instance Ord SourceLocation where
  compare (SourceLocation o1 _ _) (SourceLocation o2 _ _) = compare o1 o2

data SourceSpan = SourceSpan
  { spanStart :: SourceLocation
  , spanEnd :: SourceLocation
  }
  deriving (Eq, Ord, Show)

class Spanned a where
  getSpan :: a -> SourceSpan

mergeSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSpans (SourceSpan s1 e1) (SourceSpan s2 e2) = SourceSpan (min s1 s2) (max e1 e2)

addHorizontalOffset :: Int -> SourceLocation -> SourceLocation
addHorizontalOffset l src = src{srcLocOffset = srcLocOffset src + l, srcLocColumn = srcLocColumn src + l}

makeSpanOfLength :: Int -> SourceLocation -> SourceSpan
makeSpanOfLength l src = SourceSpan src $ addHorizontalOffset l src

makeSpanFromString :: String -> SourceLocation -> SourceSpan
makeSpanFromString s start = makeSpanFromString' s start
 where
  makeSpanFromString' [] end = SourceSpan start end
  makeSpanFromString' (x : xs) (SourceLocation o r c) = case x of
    '\n' -> makeSpanFromString' xs $ SourceLocation (o + 1) (r + 1) 1
    '\t' -> makeSpanFromString' xs $ SourceLocation (o + 1) r (c + 8)
    _ -> makeSpanFromString' xs $ SourceLocation (o + 1) r (c + 1)

(<+>) :: (Spanned a, Spanned b) => a -> b -> SourceSpan
(<+>) a b = mergeSpans (getSpan a) (getSpan b)

mergeSourceSpanWithList :: (Spanned a, Spanned b) => a -> [b] -> SourceSpan
mergeSourceSpanWithList a bs = foldl mergeSpans (getSpan a) $ map getSpan bs
