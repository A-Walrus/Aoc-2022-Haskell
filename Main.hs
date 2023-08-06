{-# LANGUAGE NamedFieldPuns #-}

import Base
import Data.Maybe
import qualified Solutions.Day01 as Day01
import qualified Solutions.Day02 as Day02
import qualified Solutions.Day03 as Day03
import qualified Solutions.Day04 as Day04
import qualified Solutions.Day05 as Day05
import qualified Solutions.Day06 as Day06
import qualified Solutions.Day15 as Day15
import qualified Solutions.Day16 as Day16
import qualified Solutions.Day17 as Day17
import qualified Solutions.Day18 as Day18
import System.Environment
import Text.Printf

days :: [String -> IO ()]
days =
  [ Day01.solution,
    Day02.solution,
    Day03.solution,
    Day04.solution,
    Day05.solution,
    Day06.solution,
    dummySolution, -- 07
    dummySolution, -- 08
    dummySolution, -- 09
    dummySolution, -- 10
    dummySolution, -- 11
    dummySolution, -- 12
    dummySolution, -- 13
    dummySolution, -- 14
    Day15.solution,
    Day16.solution,
    Day17.solution,
    Day18.solution
  ]

data Args = Args
  { day :: Int,
    path :: String
  }

parseArgs :: [String] -> Args
parseArgs [day, path] = Args {day = read day, path}
parseArgs [day] = Args {day = read day, path = "input"}
parseArgs _ = error "Not enough args"

main :: IO ()
main = do
  a <- getArgs
  let Args {day, path} = parseArgs a
  printf "AOC Day %d\n" day
  let s = days !! (day - 1)
  let filePath = printf "inputs/%02d/%s" day path
  contents <- readFile filePath
  s contents
