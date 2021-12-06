module Days.Day06 (runDay) where

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
import Data.Void
import Data.Bifunctor (bimap, first)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` string ","

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA lst = length $ periodPasses 80 lst
  where
    day 0 = [6,8]
    day n = [n-1]
    dayPasses (x:xs) = day x ++ dayPasses xs
    dayPasses [] = []
    periodPasses n = foldr1 (.) $ replicate n dayPasses

------------ PART B ------------
groupBy2 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy2 = go [] where
  go acc comp [] = acc
  go acc comp (h:t) =
    let (hs, nohs) = partition (comp h) t
    in go ((h:hs):acc) comp nohs

partB :: Input -> OutputB
partB lst = sum $ map generation $ fix 256 $ groupBy2 (==) lst
        where
          generation :: (Int, Int) -> Int
          generation (0, fish) = 2*fish
          generation (days, fish) | days < 0 = fish
          generation (days, fish) = generation (days-7, fish) + generation (days-9, fish)
          fix :: Int -> [[Int]] -> [(Int, Int)]
          fix days fishes = map (first ((days-1) -)) fish
            where
              fish = map (\x -> (head x, length x)) fishes
