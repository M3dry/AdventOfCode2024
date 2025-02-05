module Day1 where

import Data.Char (digitToInt, isNumber)
import Data.List (foldl', sort)

day1Part1 :: String -> Int
day1Part1 input = let (l1, l2) = lists input in foldl' (\acc (e1, e2) -> acc + abs (e1 - e2)) 0 $ zip (sort l1) (sort l2)

day1Part2 :: String -> Int
day1Part2 input = let (l1, l2) = lists input in foldl' (\acc e -> acc + (e * e `occurences` l2)) 0 l1

occurences :: (Eq e) => e -> [e] -> Int
occurences _ [] = 0
occurences e (e' : es)
    | e == e' = 1 + occurences e es
    | otherwise = occurences e es

lists :: String -> ([Int], [Int])
lists input =
    let (l1, l2, _) = lists' ([], []) input
     in (l1, l2)
  where
    lists' :: ([Int], [Int]) -> String -> ([Int], [Int], String)
    lists' (l1, l2) [] = (l1, l2, [])
    lists' (l1, l2) s =
        let (n1, s') = getNumber s
            s'' = trimWhitespace s'
            (n2, s''') = getNumber s''
         in lists' (n1 : l1, n2 : l2) (drop 1 s''')
    trimWhitespace [] = []
    trimWhitespace (' ' : s) = trimWhitespace s
    trimWhitespace s = s

getNumber :: String -> (Int, String)
getNumber s = getNumber' (0, s)
  where
    getNumber' :: (Int, String) -> (Int, String)
    getNumber' (num, []) = (num, "")
    getNumber' (num, n : xs)
        | isNumber n = getNumber' (num * 10 + digitToInt n, xs)
        | otherwise = (num, n : xs)
