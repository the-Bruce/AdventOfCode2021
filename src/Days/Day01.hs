module Days.Day01 (runDay) where

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
import Data.Attoparsec.Text hiding (take)
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

partA :: Input -> OutputA
partA x = sum $ map (\(a, b) -> if a<b then 1 else 0) $ zipAdj x
  where
    zipAdj x' = zip x' $ tail x'

------------ PART B ------------
partB :: Input -> OutputB
partB x = partA $ map sum $ windows 3 x
  where
    windows' n = map (take n) . tails
    windows n xs = take (length xs - n + 1) (windows' n xs)

