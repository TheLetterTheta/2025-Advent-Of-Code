module Main where

import System.Exit (die)
import Control.Applicative
import System.Environment (getArgs)
import Data.ByteString qualified as B
import Data.Attoparsec.ByteString.Char8 (decimal, endOfLine, sepBy, char, parseOnly, endOfInput, skipSpace, Parser)

-- import Debug.Trace (trace)

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures. 
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input    = [Line]  -- default to Bytestring, but very likely you'll need to change it
type Solution = Int

type Line = (Direction, Int)

data Direction = L | R deriving (Show, Eq)
parseDirection :: Parser Direction
parseDirection = 
  (char 'L' *> pure L) <|> (char 'R' *> pure R)

parseLine :: Parser Line
parseLine = (,) <$> parseDirection <*> decimal

parseInput :: Parser Input
parseInput = parseLine `sepBy` endOfLine <* skipSpace <* endOfInput

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type. 
--   this is intended to use attoparsec for such a transformation. You can use Prelude's 
--   String if it fit better for the problem
parser :: B.ByteString -> Either String Input
parser = parseOnly parseInput

-- | The function which calculates the solution for part one
rotateDial :: Int -> Line -> Int
rotateDial start line = 
  let 
    result = case fst line of
      L -> start - (snd line)
      R -> start + (snd line)
  in
  result `mod` 100

solve1 :: Input -> Solution
solve1 input = 
  length $ filter (==0) $ scanl rotateDial 50 input

-- | The function which calculates the solution for part two
clickDial :: (Int, Int) -> Line -> (Int, Int)
-- clickDial (start, count) (dir, num) | trace ("clickDial (" ++ show start ++ ", " ++ show count ++ "), (" ++ show dir ++ ", " ++ show num ++ ")") False = undefined
clickDial (start, count) (dir, num) =
  let
    rotation = case dir of
      L -> start - num
      R -> start + num
    passedZero = abs $ (rotation `quot` 100) + (if rotation <= 0 && start /= 0 then -1 else 0)
    result = rotation `mod` 100
  in
  (result, count + passedZero)

solve2 :: Input -> Solution
solve2 input =
  snd $ foldl clickDial (50, 0) input

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

