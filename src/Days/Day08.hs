module Days.Day08 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AP
import Data.Void
import Data.Char (isAlpha)
import Data.Text (Text, unpack)
{- ORMOLU_ENABLE -}

newtype ListSeven a = L7 (a,a,a,a,a,a,a) deriving (Eq, Ord, Show)

instance Functor ListSeven where
  fmap fn (L7 (a,b,c,d,e,f,g)) = L7 (fn a, fn b, fn c, fn d, fn e, fn f, fn g) 

instance Foldable ListSeven where
  foldr fn init' l7 = foldr fn init' $ asList l7

zip' :: ListSeven a -> ListSeven b -> ListSeven (a,b)
zip' (L7 (a1,b1,c1,d1,e1,f1,g1)) (L7 (a2,b2,c2,d2,e2,f2,g2)) = L7 ((a1, a2),(b1, b2),(c1, c2),(d1, d2),(e1, e2),(f1, f2),(g1, g2))

asList :: ListSeven a -> [a]
asList (L7 (a, b, c, d, e, f, g)) = [a,b,c,d,e,f,g]
fromList :: [a] -> ListSeven a
fromList [a, b, c, d, e, f, g] = L7 (a,b,c,d,e,f,g)
fromList _ = errorWithoutStackTrace "Invalid"

type SegmentDisplay = ListSeven Bool 

segDef :: SegmentDisplay
segDef = L7 (False, False, False, False, False, False, False)

setSegment :: Bool -> Char -> SegmentDisplay -> SegmentDisplay
setSegment v 'a' (L7 (_,b,c,d,e,f,g)) = L7 (v,b,c,d,e,f,g)
setSegment v 'b' (L7 (a,_,c,d,e,f,g)) = L7 (a,v,c,d,e,f,g)
setSegment v 'c' (L7 (a,b,_,d,e,f,g)) = L7 (a,b,v,d,e,f,g)
setSegment v 'd' (L7 (a,b,c,_,e,f,g)) = L7 (a,b,c,v,e,f,g)
setSegment v 'e' (L7 (a,b,c,d,_,f,g)) = L7 (a,b,c,d,v,f,g)
setSegment v 'f' (L7 (a,b,c,d,e,_,g)) = L7 (a,b,c,d,e,v,g)
setSegment v 'g' (L7 (a,b,c,d,e,f,_)) = L7 (a,b,c,d,e,f,v)
setSegment _ _ _ = errorWithoutStackTrace "Unknown Character"

addSegment :: Char -> SegmentDisplay -> SegmentDisplay
addSegment = setSegment True

getSegment :: Char -> SegmentDisplay -> Bool
getSegment 'a' (L7 (x,_,_,_,_,_,_)) = x
getSegment 'b' (L7 (_,x,_,_,_,_,_)) = x
getSegment 'c' (L7 (_,_,x,_,_,_,_)) = x
getSegment 'd' (L7 (_,_,_,x,_,_,_)) = x
getSegment 'e' (L7 (_,_,_,_,x,_,_)) = x
getSegment 'f' (L7 (_,_,_,_,_,x,_)) = x
getSegment 'g' (L7 (_,_,_,_,_,_,x)) = x
getSegment _ _ = errorWithoutStackTrace "Unknown Character"

toSegment :: Text -> SegmentDisplay
toSegment t = foldr addSegment segDef (unpack t)

arity :: Num a => SegmentDisplay -> a
arity = foldr (\b n -> if b then n+1 else n) 0

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = AP.many1 $ do
  a <- count 10 $ AP.takeWhile isAlpha <* skipSpace
  skipSpace
  _ <- string "|"
  skipSpace
  b <- count 4 $ AP.takeWhile isAlpha <* skipSpace
  skipSpace
  pure (toSegment <$> a, toSegment <$> b)
   

------------ TYPES ------------
type Input = [([SegmentDisplay],[SegmentDisplay])]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA i = sum $ map (length . filter (`elem` [2::Int,3,4,7]) . map arity . snd) i

------------ PART B ------------
patterns :: [(SegmentDisplay, Int)]
patterns = [
  (toSegment "abcefg", 0),
  (toSegment "cf", 1),
  (toSegment "acdeg", 2),
  (toSegment "acdfg", 3),
  (toSegment "bcdf", 4),
  (toSegment "abdfg", 5),
  (toSegment "abdefg", 6),
  (toSegment "acf", 7),
  (toSegment "abcdefg", 8),
  (toSegment "abcdfg", 9)
  ]

perms :: [[Char]]
perms = permutations "abcdefg"

permute' :: [Char] -> [Char] -> SegmentDisplay -> SegmentDisplay
permute' (y:ys) (x:xs) seg = setSegment (getSegment x seg) y $ permute' ys xs seg
permute' _ _ _ = segDef

permute :: [Char] -> SegmentDisplay -> SegmentDisplay
permute = permute' "abcdefg"

validPermutations :: SegmentDisplay -> [[Char]] -> [[Char]]
validPermutations segment = filter isValid
  where
    isValid p = permute p segment `elem` map fst patterns

partB :: Input -> OutputB
partB inp = sum $ zipWith decode wirings (map snd inp)
  where
    wirings = map (head . foldr validPermutations perms . fst) inp
    decode' _ _ [] = 0
    decode' m s (x:xs) = (m * unsafeLookup (permute s x) patterns) + decode' (m `div` 10) s xs
    decode = decode' 1000
    unsafeLookup a b = fromJust $ lookup a b -- Yeah, I know this is bad. Sue me... 