{-# LANGUAGE NamedFieldPuns #-}

import Base
import Data.Maybe
import Solutions.Day01 qualified as Day01
import Solutions.Day02 qualified as Day02
import Solutions.Day03 qualified as Day03
import Solutions.Day04 qualified as Day04
import Solutions.Day05 qualified as Day05
import Solutions.Day06 qualified as Day06
import Solutions.Day07 qualified as Day07
import Solutions.Day08 qualified as Day08
import Solutions.Day09 qualified as Day09
import Solutions.Day10 qualified as Day10
import Solutions.Day15 qualified as Day15
import Solutions.Day16 qualified as Day16
import Solutions.Day17 qualified as Day17
import Solutions.Day18 qualified as Day18
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
    Day07.solution,
    Day08.solution,
    Day09.solution,
    Day10.solution,
    dummySolution, -- 11
    dummySolution, -- 12
    dummySolution, -- 13
    dummySolution, -- 14
    Day15.solution,
    Day16.solution,
    Day17.solution,
    Day18.solution
  ]

data Args
  = RunDay
      { day :: Int,
        path :: Maybe String
      }
  | All

parseArgs :: [String] -> Args
parseArgs [] = All
parseArgs ["all"] = All
parseArgs [day] = RunDay {day = read day, path = Nothing}
parseArgs [day, path] = RunDay {day = read day, path = Just path}
parseArgs _ = error "Not enough args"

main :: IO ()
main = do
  a <- getArgs
  let args = parseArgs a
  runArgs args

runArgs :: Args -> IO ()
runArgs RunDay {day, path} = runDay day path
runArgs All = mapM_ (`runDay` Nothing) [1..]

runDay :: Int -> Maybe String -> IO ()
runDay day path = do
  printf " -- AOC Day %d -- \n" day
  let path' = fromMaybe "input" path
  let solution = days !! (day - 1)
  let filePath = printf "inputs/%02d/%s" day path'
  contents <- readFile filePath
  solution contents
  putStrLn ""
