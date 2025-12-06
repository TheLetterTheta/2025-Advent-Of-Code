module Main where

import System.Exit (die)
import System.Environment (getArgs)
import Data.ByteString qualified as B
import Data.Attoparsec.ByteString.Char8 (decimal, sepBy, endOfLine, char, parseOnly, endOfInput, skipSpace, Parser)
import Data.Range qualified as R

-- import Debug.Trace

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures. 
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Solution = Integer

data Input = Input { ranges :: [R.Range Integer], fruits :: [Integer] } deriving (Show)

parseRange :: Parser (R.Range Integer)
parseRange = (R.+=+) <$> decimal <* char '-' <*> decimal

parseInput :: Parser Input
parseInput = do
  ranges <- (parseRange `sepBy` endOfLine) <* skipSpace
  fruits <- (decimal `sepBy` endOfLine) <* skipSpace <* endOfInput
  return Input { ranges = ranges, fruits = fruits }

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type. 
--   this is intended to use attoparsec for such a transformation. You can use Prelude's 
--   String if it fit better for the problem
parser :: B.ByteString -> Either String Input
parser = parseOnly parseInput

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 Input { ranges , fruits } =
  fromIntegral . length $ filter (R.inRanges joinedRanges) fruits
  where
    joinedRanges = R.joinRanges ranges

-- | The function which calculates the solution for part two
sizeOfRange :: R.Range Integer -> Integer
sizeOfRange = \case
  R.SpanRange (R.Bound from _) (R.Bound to _) -> to - from + 1
  R.SingletonRange _ -> 1
  _ -> 0

solve2 :: Input -> Solution
solve2 Input { ranges } =
  sum . map sizeOfRange $ R.joinRanges ranges

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


