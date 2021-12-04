module Days.Day03 (runDay) where

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
import Data.Functor (($>))
import Data.Attoparsec.ByteString.Char8 (isDigit)
import Data.Text (unpack)
{- ORMOLU_ENABLE -}

type Binary = [Int]
binToDec :: Binary -> Int
binToDec = binToDec' 0
  where
    binToDec' a (b:xs) = binToDec' (a*2 + b) xs
    binToDec' a [] = a

decToBin :: Int -> Binary
decToBin i = i `div` 2 : decToBin (i `mod` 2)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy binary endOfLine
  where
    binary = do
      a <- AP.takeWhile isDigit
      pure (map asInt (unpack a))
    asInt :: Char -> Int
    asInt '0' = 0
    asInt '1' = 1
    asInt _ = error "Invalid Char"


------------ TYPES ------------
type Input = [Binary]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA inp = binToDec (map epsilon val) * binToDec (map gamma val)
  where
    len = length inp
    val = map sum $ transpose inp
    epsilon x = if x>(len `div` 2) then 1 else 0
    gamma x = if x<(len `div` 2) then 1 else 0
    

------------ PART B ------------
partB :: Input -> OutputB
partB inp = binToDec (recFilterPredicate mostCommon 0 inp) * binToDec (recFilterPredicate leastCommon 0 inp)
  where
    recFilterPredicate :: (Int -> Int -> Int) -> Int -> [Binary] -> Binary
    recFilterPredicate _ _ [x] = x
    recFilterPredicate pred' pos lst = recFilterPredicate pred' (pos+1) $ filterRowPredicate (pred' $ length lst) pos lst
    filterRowPredicate pred' pos lst = filterRows (pred' (val lst !! pos)) pos lst
    val lst = map sum $ transpose lst
    mostCommon len x = if x>=((len+1) `div` 2) then 1 else 0
    leastCommon len x = if x<((len+1) `div` 2) then 1 else 0
    filterRows :: Int -> Int -> [Binary] -> [Binary]
    filterRows needle pos lst = filter ((== needle) . (!! pos)) lst
