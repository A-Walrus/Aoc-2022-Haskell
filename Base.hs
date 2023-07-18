{-# LANGUAGE NamedFieldPuns #-}

module Base where

import Debug.Trace
import Data.Bifunctor

debug :: Show a => String -> a -> a
debug s v = trace (s ++ ": " ++ show v) v

dMap f = bimap f f


data Day a b c = Day {parse' :: String -> a, part1' :: a -> b, part2' :: a -> c}

run :: (Show b, Show c) => Day a b c -> String -> IO ()
run Day {parse', part1', part2'} s = do
  let parsed = parse' s
  putStr "Part1: "
  print (part1' parsed)
  putStr "Part2: "
  print (part2' parsed)
