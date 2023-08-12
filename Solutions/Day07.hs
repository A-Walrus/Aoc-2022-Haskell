module Solutions.Day07 (solution) where

import Base
import Data.List
import Data.Map qualified as Map
import Data.Maybe

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = File

parse :: String -> Parsed
parse = fst . foldl f (Dir Map.empty, []) . lines
  where
    f (map, cwd) s = case words s of
      ["$", "cd", "/"] -> (map, [])
      ["$", "cd", ".."] -> (map, init cwd)
      ["$", "cd", path] -> (map, cwd ++ [path])
      ["$", "ls"] -> (map, cwd)
      ["dir", name] -> (insertFile map (cwd ++ [name]) (Dir Map.empty), cwd)
      [size, name] -> (insertFile map (cwd ++ [name]) (Regular (read size)), cwd)

insertFile :: File -> Path -> File -> File
insertFile (Dir map) [name] file = Dir (Map.insert name file map)
insertFile (Dir map) (name : xs) file = Dir (Map.update (\dir -> Just $ insertFile dir xs file) name map)

part1 :: Parsed -> Int
part1 = sumAtMost

part2 :: Parsed -> Int
part2 dir = smallestAbove needToFree dir
  where
    needToFree = size dir - 40000000

type Path = [String]

data File = Dir (Map.Map String File) | Regular Int deriving (Show)

size :: File -> Int
size (Regular x) = x
size (Dir map) = sum $ Map.map size map

sumAtMost :: File -> Int
sumAtMost (Regular _) = 0
sumAtMost dir@(Dir children) =
  childSolve
    + if size dir > 100000
      then 0
      else size dir
  where
    childSolve = sum $ Map.map sumAtMost children

smallestAbove :: Int -> File -> Int
smallestAbove n (Regular _) = maxBound
smallestAbove n dir@(Dir children)
  | size dir >= n = min (size dir) (minimum (map (smallestAbove n) (Map.elems children)))
  | otherwise = maxBound
