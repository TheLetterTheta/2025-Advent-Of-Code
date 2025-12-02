module Main where

import System.Exit (die)
import Control.Applicative
import System.Environment (getArgs)
import Data.ByteString qualified as B
import Data.List.Ordered
import Data.Attoparsec.ByteString.Char8 (decimal, endOfLine, sepBy, char, parseOnly, endOfInput, skipSpace, Parser)
import Math.NumberTheory.Logarithms

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures. 
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input    = [Range]  -- default to Bytestring, but very likely you'll need to change it
type Solution = Integer

data Range = Range { from :: Integer, to :: Integer } deriving (Show)

parseRange :: Parser Range
parseRange =
  do
  [from , to] <- decimal `sepBy` (char '-')
  return Range { from = from, to = to }

parseInput :: Parser Input
parseInput = parseRange `sepBy` (char ',') <* skipSpace <* endOfInput

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type. 
--   this is intended to use attoparsec for such a transformation. You can use Prelude's 
--   String if it fit better for the problem
parser :: B.ByteString -> Either String Input
parser = parseOnly parseInput


-- | The function which calculates the solution for part one
nums :: [Integer]
nums = [1..]

makeDoubled :: Integer -> Integer
makeDoubled i =
  i * 10 ^ (1 + integerLog10 i) + i

repeatedNums :: [Integer]
repeatedNums = map makeDoubled nums

findRange :: Range -> [Integer]
findRange (Range { from = from , to = to }) = 
  filter (>= from) (takeWhile (<= to) repeatedNums)

solve1 :: Input -> Solution
solve1 input =
  sum $ concatMap findRange input

-- | The function which calculates the solution for part two
repeatNum :: Integer -> [Integer]
repeatNum i = iterate (\x -> x * 10 ^ (1 + integerLog10 i) + i) i

repeatAll :: [Integer]
repeatAll =
  mergeAll $ map (\i -> tail (repeatNum i)) [1 ..]

findAllRepeat :: Range -> [Integer]
findAllRepeat (Range { from = from, to = to}) =
  filter (>= from) (takeWhile (<= to) repeatAll)

dedup :: Eq a =>[a] -> [a]
dedup [] = []
dedup [x] = [x]
dedup (x:y:xs)
  | x == y = dedup (y:xs)
  | otherwise = x : dedup(y:xs)

solve2 :: Input -> Solution
solve2 input = 
  sum $ dedup $ concatMap findAllRepeat input

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  content <- B.readFile filepath
  case parser content of -- use parser <$> readFile filepath if String is better
    Left err -> die $ "Parse error: " ++ err
    Right input ->
      if read @Int part == 1
        then do
          putStrLn "solution to problem 1 is:"
          print $ solve1 input
        else do
          putStrLn "solution to problem 2 is:"
          print $ solve2 input

