module Days.Day02 (runDay) where

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
import Data.Functor (($>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

data SubAction = Forward Int | Up Int | Down Int deriving (Show, Eq)

type Location = (Int, Int)
type Pose = (Location, Int)

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy operation endOfLine
  where
    operation = do
      act <- choice [string "forward" $> Forward, string "up" $> Up, string "down" $> Down]
      skipSpace
      act <$> decimal


------------ TYPES ------------
type Input = [SubAction]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA inp = locSum $ foldl' updateState (0,0) inp
  where
    locSum (x, z) = x*z
    updateState :: Location -> SubAction -> Location
    updateState (x, z) (Forward i) = (x+i, z)
    updateState (x, z) (Up i) = (x, z-i)
    updateState (x, z) (Down i) = (x, z+i)

------------ PART B ------------
partB :: Input -> OutputB
partB inp = poseSum $ foldl' updateState ((0,0),0) inp
  where
    poseSum (loc, _) = locSum loc
    locSum (x, z) = x*z
    updateState :: Pose -> SubAction -> Pose
    updateState ((x, z), aim) (Forward i) = ((x+i, z+(aim*i)), aim)
    updateState (loc, aim) (Up i) = (loc, aim-i)
    updateState (loc, aim) (Down i) = (loc, aim+i)