module Days.Day04 (runDay) where

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
import Control.Monad
import Data.Functor ((<&>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Board = [[(Int, Bool)]]

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  numbers <- sepBy decimal ","
  skipSpace
  boards <- sepBy parseBoard skipSpace
  pure (numbers, boards)
  where
    parseBoard :: Parser Board
    parseBoard = replicateM 5 $ replicateM 5 $ (decimal <&> (,False)) <* skipSpace

------------ TYPES ------------
type Input = ([Int], [Board])

type OutputA = Int

type OutputB = Int

boolBoard :: Board -> [[Bool]]
boolBoard = fmap $ fmap snd

intBoard :: Board -> [[Int]]
intBoard = fmap $ fmap fst

score :: (Board, Int) -> Int
score (b, a) = a * sum (unmarked <$> concat b)
  where
    unmarked (a', False) = a'
    unmarked _ = 0

isWinning :: Board -> Bool
isWinning a = or (fmap and board) || or (fmap and (transpose board))
  where
    board = boolBoard a

hasWinning :: [Board] -> Maybe Board
hasWinning = find isWinning

doMark :: Int -> [Board] -> [Board]
doMark i = fmap (fmap $ fmap (mark i))

mark :: Eq a => a -> (a, Bool) -> (a, Bool)
mark i (x, False) | i == x = (x, True)
mark _ x = x

------------ PART A ------------
partA :: Input -> OutputA
partA = score . fromJust . uncurry untilWinning
  where
    untilWinning :: [Int] -> [Board] -> Maybe (Board, Int)
    untilWinning [] _ = Nothing
    untilWinning (a : xs) boards = if isJust win then Just (fromJust win, a) else untilWinning xs (doMark a boards)
      where
        win = hasWinning (doMark a boards)

------------ PART B ------------
partB :: Input -> OutputB
partB (l, b) = partA $ fromJust $ untilLoosing l b
  where
    untilLoosing :: [Int] -> [Board] -> Maybe ([Int], [Board])
    untilLoosing remaining [a] = Just (remaining, [a])
    untilLoosing [] _ = Nothing
    untilLoosing (a : xs) boards = if isJust win then untilLoosing xs lose else untilLoosing xs (doMark a boards)
      where
        win = hasWinning (doMark a boards)
        lose = filter (not . isWinning) (doMark a boards)
