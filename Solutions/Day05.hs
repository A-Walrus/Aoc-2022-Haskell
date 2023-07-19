{-# LANGUAGE NamedFieldPuns #-}

module Solutions.Day05 (solution) where

import Base
import Data.List
import Data.Maybe

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = ([[Char]], [Command])

parse :: String -> Parsed
parse s = (parseCrates crates, map parseCommand $ drop 1 commands)
  where
    liness = lines s
    (crates, commands) = splitAt (fromJust (elemIndex "" liness)) liness

part1 :: Parsed -> String
part1 (state, commands) = top $ foldl (apply f) state commands
  where
    f amount = take amount . reverse

part2 :: Parsed -> String
part2 (state, commands) = top $ foldl (apply f) state commands
  where
    f amount l = drop (length l - amount) l

parseCrates :: [String] -> [[Char]]
parseCrates lines = map (filter (/= ' ')) $ rotate crates
  where
    crateLines = init lines
    crates = map (everyf 4 . drop 1) crateLines
    rotate = transpose . reverse
    rotated = rotate crates

everyf n [] = []
everyf n as = head as : everyf n (drop n as)

parseCommand :: String -> Command
parseCommand s = Command {amount, from, to}
  where
    words' = words s
    [_, amount, _, from, _, to] = map read words'

data Command = Command
  { amount :: Int,
    from :: Int,
    to :: Int
  }

top :: [[Char]] -> [Char]
top = map lastOr
  where
    lastOr [] = ' '
    lastOr l = last l

apply :: (Int -> [Char] -> [Char]) -> [[Char]] -> Command -> [[Char]]
apply takeF state Command {amount, from, to} = map f enumerated
  where
    enumerated = zip [1 ..] state -- 1 index enumeration
    fromList = state !! (from - 1) -- switch to 0 indexed for
    taken = takeF amount fromList
    f (i, l)
      | i == to = l ++ taken
      | i == from = take (length l - amount) l
      | otherwise = l
