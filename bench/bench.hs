module Main where

import Criterion.Main
import qualified Data.ByteString as B
import YourModule (parser, solve1, solve2)

main :: IO ()
main = do
  input <- B.readFile "./input/day-1.txt"
  let parsed = case parser input of
        Left err -> error err
        Right x -> x
  
  defaultMain
    [ bgroup "day-1"
        [ bench "parse" $ nf parser input
        , bench "solve1" $ nf solve1 parsed
        , bench "solve2" $ nf solve2 parsed
        , bench "full-part1" $ nfIO $ do
            content <- B.readFile "./input/day-1.txt"
            case parser content of
              Left _ -> return 0
              Right p -> return $ solve1 p
        ]
    ]
