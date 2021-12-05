module Days.Day05 (runDay) where

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
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Point = (Int, Int)

type Line = (Point, Point)

type Space = [(Point, Int)]

------------ PARSER ------------
inputParser :: Parser Input
inputParser = line `sepBy` skipSpace
  where
    line = do
      x1 <- decimal
      _ <- string ","
      y1 <- decimal
      _ <- string " -> "
      x2 <- decimal
      _ <- string ","
      y2 <- decimal
      pure ((x1, y1), (x2, y2))

------------ TYPES ------------
type Input = [Line]

type OutputA = Int

type OutputB = Int

isOnLine :: Line -> Point -> Bool
isOnLine ((x1, y1), (x2, y2)) (x, y)
  | x1 == x2 = x == x1 && y <= max y1 y2 && y >= min y1 y2
  | y1 == y2 = y == y1 && x <= max x1 x2 && x >= min x1 x2
  | otherwise = sx * (x - x1) == sy * (y - y1) && y <= max y1 y2 && y >= min y1 y2 && x <= max x1 x2 && x >= min x1 x2
  where
    dx = x1 - x2
    dy = y1 - y2
    sx = signum dx
    sy = signum dy

initialSpace :: Space
initialSpace = [((x, y), 0) | y <- [0 .. 1000], x <- [0 .. 1000]]

------------ PART A ------------
partA :: Input -> OutputA
partA inp = length $ filter ((>= 2) . snd) $ foldr markSpace initialSpace (filter orthog inp)
  where
    orthog :: Line -> Bool
    orthog ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2
    markSpace :: Line -> Space -> Space
    markSpace l s = map (updatePoint l) s
    updatePoint :: Line -> (Point, Int) -> (Point, Int)
    updatePoint l (p, b) = (p, b + (if isOnLine l p then 1 else 0))

------------ PART B ------------
partB :: Input -> OutputB
partB inp = length $ filter ((>= 2) . snd) $ foldr markSpace initialSpace inp
  where
    markSpace :: Line -> Space -> Space
    markSpace l s = map (updatePoint l) s
    updatePoint :: Line -> (Point, Int) -> (Point, Int)
    updatePoint l (p, b) = (p, b + (if isOnLine l p then 1 else 0))
