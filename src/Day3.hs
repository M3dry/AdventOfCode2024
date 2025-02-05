module Day3 where

import Data.Char

day3Part1 :: String -> Int
day3Part1 [] = 0
day3Part1 ('m' : 'u' : 'l' : '(' : xs) =
    let (n1, xs') = parseNum xs
     in case xs' of
            (',' : rest) ->
                let (n2, rest') = parseNum rest
                 in case rest' of
                        (')' : rest') -> n1 * n2 + day3Part1 rest'
                        _ -> day3Part1 rest'
            _ -> day3Part1 xs'
day3Part1 (_ : xs) = day3Part1 xs

day3Part2 :: String -> Int
day3Part2 = day3Part2' True
  where
    day3Part2' :: Bool -> String -> Int
    day3Part2' _ [] = 0
    day3Part2' _ ('d' : 'o' : 'n' : '\'': 't' : '(' : ')' : xs) = day3Part2' False xs
    day3Part2' _ ('d' : 'o' : '(' : ')' : xs) = day3Part2' True xs
    day3Part2' True ('m' : 'u' : 'l' : '(' : xs) =
        let (n1, xs') = parseNum xs
         in case xs' of
                (',' : rest) ->
                    let (n2, rest') = parseNum rest
                     in case rest' of
                            (')' : rest') -> n1 * n2 + day3Part2' True rest'
                            _ -> day3Part2' True rest'
                _ -> day3Part2' True xs'
    day3Part2' cond (_ : xs) = day3Part2' cond xs

parseNum :: String -> (Int, String)
parseNum input = parseNum' (0, input)
  where
    parseNum' :: (Int, String) -> (Int, String)
    parseNum' (n, []) = (n, [])
    parseNum' (n, x : xs)
        | isDigit x = parseNum' (n * 10 + digitToInt x, xs)
        | otherwise = (n, x : xs)
