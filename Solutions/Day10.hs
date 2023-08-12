module Solutions.Day10 (solution) where

import Base
import Data.List
import Data.List.Split

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [Int]

parse :: String -> Parsed
parse = runCmds 1 . map (command . words) . lines
  where
    command ["noop"] = Noop
    command ["addx", val] = AddX (read val)

part1 :: Parsed -> Int
part1 s = sum vals
  where
    positions = [20, 60 .. 220]
    vals = map (signalStrength s) positions

signalStrength :: [Int] -> Int -> Int
signalStrength signal index = index * signal !! (index - 1)

part2 :: Parsed -> String
part2 = render . pixels

runCmds :: Int -> [Command] -> [Int]
runCmds x [] = []
runCmds x (Noop : cs) = x : runCmds x cs
runCmds x ((AddX val) : cs) = x : x : runCmds (x + val) cs

data Command = Noop | AddX Int deriving (Show)



pixels :: [Int] -> [Bool]
pixels = zipWith check xPos
  where
    index = [0 ..]
    xPos = map (`mod` 40) index
    check a b = abs (a - b) < 2

render :: [Bool] -> String
render pixels = '\n': intercalate "\n" lines
  where
    pixel True = '#'
    pixel False = '.'
    lines = map (map pixel) (chunksOf 40 pixels)
