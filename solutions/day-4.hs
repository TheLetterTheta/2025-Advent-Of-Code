module Main where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (many1, sepBy, parseOnly, endOfLine, endOfInput, skipSpace, char, Parser)
import Data.ByteString qualified as B
import Data.Matrix qualified as M
import Data.Vector qualified as V
import System.Environment (getArgs)
import System.Exit (die)

-- import Debug.Trace 

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures. 
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input    = M.Matrix Contents
type Solution = Int

data Contents = Paper | Empty deriving (Show, Eq)

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type. 
--   this is intended to use attoparsec for such a transformation. You can use Prelude's 
--   String if it fit better for the problem
parseInput :: Parser [[Contents]]
parseInput = sepBy (many1 parsePaper) endOfLine <* skipSpace <* endOfInput
  where
    parsePaper = (Empty <$ char '.') <|> (Paper <$ char '@')

parser :: B.ByteString -> Either String Input
parser = fmap M.fromLists . parseOnly parseInput

-- | The function which calculates the solution for part one
countPapers :: M.Matrix Contents -> Int
countPapers =
  V.length . V.filter isPaper . M.getMatrixAsVector
  where
    isPaper :: Contents -> Bool
    isPaper Paper = True
    isPaper Empty = False

-- Generate all 3x3 submatrix positions
submatrixPositions :: Int -> Int -> [((Int, Int), (Int, Int))]
submatrixPositions rows cols = 
    [ ((r, r + 2), (c, c + 2))
    | r <- [1 .. rows - 2]
    , c <- [1 .. cols - 2]
    ]

-- Add padding to matrix (border of Empty cells)
addPadding :: M.Matrix Contents -> M.Matrix Contents
addPadding m = topRow M.<-> (leftCol M.<|> paddedMatrix)
  where
    rows = M.nrows m
    cols = M.ncols m
    topRow        = M.matrix 1 (cols + 2) (const Empty)
    leftCol       = M.matrix (rows + 1) 1 (const Empty)
    paddedMatrix  = M.extendTo Empty (rows + 1) (cols + 1) m

-- Extract and validate submatrix
validSubmatrix :: M.Matrix Contents -> ((Int, Int), (Int, Int)) -> Maybe (M.Matrix Contents)
validSubmatrix input ((rStart, rEnd), (cStart, cEnd)) =
    let sub = M.submatrix rStart rEnd cStart cEnd input
    in if M.getElem 2 2 sub == Paper then Just sub else Nothing

solve1 :: Input -> Solution
solve1 = 
    length 
    . filter (< 5) 
    . map countPapers 
    . getValidSubmatrices 
    . addPadding
  where
    getValidSubmatrices input = 
        [ sub
        | pos <- submatrixPositions (M.nrows input) (M.ncols input)
        , Just sub <- [validSubmatrix input pos]
        ]

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = error "Not implemented"

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

