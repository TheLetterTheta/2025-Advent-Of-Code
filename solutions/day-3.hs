module Main where

import System.Exit (die)
import System.Environment (getArgs)
import Data.ByteString qualified as B
import Data.Char (digitToInt)
import Data.Attoparsec.ByteString.Char8 (digit, many1, sepBy, parseOnly, endOfLine, endOfInput, skipSpace, Parser)
import Data.List (scanl', foldl')

-- import Debug.Trace 

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures. 
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input    = [[Int]]  -- default to Bytestring, but very likely you'll need to change it
type Solution = Int

parseInput :: Parser Input
parseInput = (many1 parseDigitAsInt) `sepBy` endOfLine <* skipSpace <* endOfInput
  where
    parseDigitAsInt = digitToInt <$> digit

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type. 
--   this is intended to use attoparsec for such a transformation. You can use Prelude's 
--   String if it fit better for the problem
parser :: B.ByteString -> Either String Input
parser = parseOnly parseInput

-- | The function which calculates the solution for part one
computeNewMax :: (Int, Int) -> Int -> (Int, Int)
-- computeNewMax _ _ (n,i) | trace ("compute max" ++ show n ++ " " ++ show i) False = undefined
computeNewMax (_, curr_mag) n =
    (curr_mag * 10 + n, max n curr_mag)

findMaxNum :: [Int] -> Int
findMaxNum = maximum . map fst . scanl' computeNewMax (0, 0)

solve1 :: Input -> Solution
solve1 = sum . map findMaxNum

-- | The function which calculates the solution for part two
getHighestBefore :: Int -> [Int] -> [Int]
getHighestBefore 0 n = [maximum n]
getHighestBefore n nums = 
  let
    highestValue = maximum (take (length nums - n) nums)
  in
  highestValue : getHighestBefore (n-1) (drop 1 $ dropWhile (< highestValue) nums)


solve2 :: Input -> Solution
solve2 = sum . map (foldl' (\acc n -> acc * 10 + n) 0 .  getHighestBefore 11 )

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

