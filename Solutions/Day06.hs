module Solutions.Day06 (solution) where

import Base
import Data.List
import Data.Maybe

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = String

parse :: String -> Parsed
parse = id

part1 :: Parsed -> Int
part1 = solve 4

part2 :: Parsed -> Int
part2 = solve 14

solve n = (+n) . fromJust . findIndex distinct . windows n

windows :: Int -> [a] -> [[a]]
windows n l = foldr (\x (l : ls) -> init (x : l) : l : ls) [start] end
  where
    end = take (length l - n) l
    start = drop (length l - n) l

distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs