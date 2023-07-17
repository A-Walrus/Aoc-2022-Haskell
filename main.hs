{-# LANGUAGE NamedFieldPuns #-}

import qualified Solutions.Day01 as Day01
import qualified Solutions.Day02 as Day02

import Data.Maybe
import System.Environment
import Text.Printf


days :: [String -> IO ()]
days = [
  Day01.solution,
  Day02.solution
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
