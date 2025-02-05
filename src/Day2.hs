module Day2 where

import Data.Foldable (foldl')
import Data.List (inits, tails)

day2Part1 :: String -> Int
day2Part1 input =
    let x = map (map (read @Int) . words) . lines $ input
     in foldl' (\acc r -> acc + fromEnum (checkSafety r)) 0 x

day2Part2 :: String -> Int
day2Part2 input =
    let x = map (map (read @Int) . words) . lines $ input
     in foldl' (\acc r -> let y = checkSafety' r in acc + fromEnum y) 0 x

windowed :: Int -> [a] -> [[a]]
windowed _ [] = []
windowed size ls@(_ : xs)
    | length ls >= size = take size ls : windowed size xs
    | otherwise = windowed size xs

checkSafety :: [Int] -> Bool
checkSafety [] = True
checkSafety row =
    let ints = map (\[x, y] -> x - y) $ windowed 2 row
        noZero = (0 `notElem` ints)
        betweenRange = all (\i -> abs i > 0 && abs i < 4) ints
        incr = all (> 0) ints
        decr = all (0 >) ints
     in noZero && betweenRange && (incr || decr)

checkSafety' :: [Int] -> Bool
checkSafety' = any checkSafety . permutations

permutations :: [a] -> [[a]]
permutations xs = xs : zipWith (++) (inits xs) (tail $ tails xs)
