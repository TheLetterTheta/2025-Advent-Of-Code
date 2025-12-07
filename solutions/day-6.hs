module Main where

import Control.Applicative
import Data.Char (digitToInt)
import Data.Maybe (catMaybes)
import System.Exit (die)
import System.Environment (getArgs)
import Data.ByteString qualified as B
import Data.Attoparsec.ByteString.Char8 as A (decimal, digit, many', sepBy, endOfLine, char, parseOnly, endOfInput, skipSpace, Parser)
import Data.List

-- import Debug.Trace

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures. 
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Solution = Integer
data Input = Input { numbers :: [[Integer]], functions :: [Operator] } deriving (Show)
data InputTwo = InputTwo { chars :: [[Maybe Integer]], operators :: [Operator] } deriving (Show)

data Operator = Times | Add deriving (Show, Eq)

readSpaces :: Parser [Char]
readSpaces = many' $ char ' '

parseNums :: Parser [Integer]
parseNums = decimal `sepBy` readSpaces

parseMaybeNums :: Parser [Maybe Integer]
parseMaybeNums = many' $ (Just . fromIntegral . digitToInt <$> digit) <|> (Nothing <$ char ' ')

parseOperators :: Parser [Operator]
parseOperators = parseOperator `sepBy` readSpaces
  where
    parseOperator = (Times <$ char '*') <|> (Add <$ char '+')

parseInput :: Parser Input
parseInput = do
  nums <- parseNums `sepBy` (readSpaces *> endOfLine)
  fs <- parseOperators <* skipSpace <* endOfInput
  return Input { numbers = nums, functions = fs }

parseInputTwo :: Parser InputTwo
parseInputTwo = do
  nums <- parseMaybeNums `sepBy` (readSpaces *> endOfLine)
  fs <- parseOperators <* skipSpace <* endOfInput
  return InputTwo { chars = nums, operators = fs }

parserOne :: B.ByteString -> Either String Input
parserOne = parseOnly parseInput

-- Second parser because input is strange
parserTwo :: B.ByteString -> Either String InputTwo
parserTwo = parseOnly parseInputTwo

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 Input { numbers, functions } =
  let
  nums = transpose numbers
  equations = zip nums functions
  in
  sum (map (\(n, f) -> 
   case f of
     Times -> product n 
     Add -> sum n) equations)

-- | The function which calculates the solution for part two
maybeNumsToInt :: [Maybe Integer] -> Integer
maybeNumsToInt = foldl (\acc n -> acc * 10 + n) 0 . catMaybes

groupListSeparatedByZeros :: [Integer] -> [[Integer]]
groupListSeparatedByZeros [] = []
groupListSeparatedByZeros list =
  let
  (g, r) = break (== 0) list
  in
  g : groupListSeparatedByZeros (drop 1 r)

solve2 :: InputTwo -> Solution
solve2 InputTwo { chars, operators } = 
  let
  nums = groupListSeparatedByZeros $ map maybeNumsToInt $ transpose chars
  equations = zip nums operators
  in
  sum (map (\(n, f) -> 
   case f of
     Times -> product n 
     Add -> sum n) equations)

main :: IO ()
main = do
  [part, filepath] <- getArgs
  content <- B.readFile filepath
  if read @Int part == 1
  then do
    case parserOne content of -- use parserOne <$> readFile filepath if String is better
      Left err -> die $ "Parse error: " ++ err
      Right input -> do
        putStrLn "solution to problem 1 is:"
        print $ solve1 input
  else do
    case parserTwo content of -- use parserOne <$> readFile filepath if String is better
      Left err -> die $ "Parse error: " ++ err
      Right input -> do
        putStrLn "solution to problem 2 is:"
        print $ solve2 input
