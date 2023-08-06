module Solutions.Day16 (solution) where

import Base
import Data.List
import Data.Map (Map, adjust, keysSet, (!))
import qualified Data.Map as Map
import Debug.Trace


solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = Valves

parse :: String -> Parsed
parse = process . map parseLine . lines
  where
    process = Map.fromList . map (\Line {from, connectedTo, flowRate} -> (from, Valve {open = False, to = connectedTo, rate = flowRate}))

    parseLine :: String -> Line
    parseLine = fromWords . words
      where
        fromWords :: [String] -> Line
        fromWords (_ : from : _ : _ : rate : _ : _ : _ : _ : xs) =
          Line
            { from = Id from,
              flowRate = (read . init . drop 5) rate,
              connectedTo = map (Id . filter (/= ',')) xs
            }

part1 :: Parsed -> Int
part1 v = map (maximum . map released) steps !! 30
  where
    start = state v
    steps = iterate (cull . (step =<<)) [start]


part2 :: Parsed -> Int
part2 = undefined


newtype Id = Id String deriving (Show, Ord, Eq)

data Line = Line {from :: Id, connectedTo :: [Id], flowRate :: Int} deriving (Show)

data Valve = Valve {open :: Bool, rate :: Int, to :: [Id]} deriving (Show, Eq, Ord)

type Valves = Map Id Valve

data State = State {position :: Id, valves :: Valves, released :: Int} deriving (Show, Eq, Ord)

state :: Valves -> State
state valves = State {valves, released = 0, position = Id "AA"}


openValves = Map.filter open

step :: State -> [State]
step s@State {position, valves, released}
  | open current || rate current == 0 = moves
  | otherwise = opened : moves
  where
    current = valves ! position
    releasedNow = (sum . Map.map rate . openValves) valves
    newState = s {released = released + releasedNow}
    moves = (map (\p -> newState {position = p}) . to) current
    opened = newState {valves = adjust (\v -> v {open = True}) position valves}

cull :: [State] -> [State]
cull states = map (\(s, r) -> s {released = r}) $ Map.toList m
  where
    s = map (\state@State {released} -> (state {released = 0}, released)) states
    m = Map.fromListWith max s

