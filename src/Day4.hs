module Day4 where

import Control.Monad (when)
import Data.Array (Array, bounds, listArray, (!))
import Data.List (elemIndex, foldl')
import Utility (dedup)

day4Part1 :: String -> Int
day4Part1 input =
    let chars = lines input
        (maxY, maxX) = (length chars, length $ head chars)
        ix = ((1, 1), (maxY, maxX))
        arr = listArray ix $ concat chars
        accumed = foldl' (\acc y -> acc ++ foldl' (\acc' x -> acc' ++ xmasAtPoint arr (y, x)) [] [1 .. maxX]) [] [1 .. maxY]
     in length $ dedup accumed

day4Part2 :: String -> Int
day4Part2 input =
    let chars = lines input
        (maxY, maxX) = (length chars, length $ head chars)
        ix = ((1, 1), (maxY, maxX))
        arr = listArray ix $ concat chars
        accumed = foldl' (\acc y -> acc ++ foldl' (\acc' x -> if masAtPoint arr (y, x) then (y, x) : acc' else acc') [] [1 .. maxX]) [] [1 .. maxY]
     in length $ dedup accumed

xmasAtPoint :: Array (Int, Int) Char -> (Int, Int) -> [[(Int, Int)]]
xmasAtPoint arr point@(y, x) =
    case (arr ! point) `elemIndex` xmasLetters of
        Just idx ->
            let horizLR = [(y, x') | x' <- [x - idx .. 3 + x - idx]]
                reverseIdx = 3 - idx
                horizRL = [(y, x') | x' <- [3 + x - reverseIdx, 2 + x - reverseIdx .. x - reverseIdx]]
                vertTD = [(y', x) | y' <- [y - idx .. 3 + y - idx]]
                vertDT = [(y', x) | y' <- [3 + y - reverseIdx, 2 + y - reverseIdx .. y - reverseIdx]]
                diagLRIncr = zip (reverse [y - idx - 3 .. y - idx]) [x - idx .. 3 + x - idx]
                diagLRDecr = zip [y - idx .. y - idx + 3] [x - idx .. x - idx + 3]
                diagRLIncr = reverse $ zip [y - idx - 3 .. y - idx] [x - idx - 3 .. x - idx]
                diagRLDecr = zip [y - idx .. 3 + y - idx] (reverse [x - idx - 3 .. x - idx])
             in filter (checkSequence arr) [horizLR, horizRL, vertTD, vertDT, diagLRIncr, diagLRDecr, diagRLIncr, diagRLDecr]
        Nothing -> []

masAtPoint :: Array (Int, Int) Char -> (Int, Int) -> Bool
masAtPoint arr point@(y, x)
    | arr ! point == 'A' && (x /= 1 && x /= maxX && y /= 1 && y /= maxY) && checkMS = True
    | otherwise = False
  where
    (_, (maxY, maxX)) = bounds arr
    checkMS =
        ((arr ! (y + 1, x - 1) == 'M' && arr ! (y - 1, x + 1) == 'S') || (arr ! (y + 1, x - 1) == 'S' && arr ! (y - 1, x + 1) == 'M'))
            && ((arr ! (y + 1, x + 1) == 'M' && arr ! (y - 1, x - 1) == 'S') || (arr ! (y + 1, x + 1) == 'S' && arr ! (y - 1, x - 1) == 'M'))

checkSequence :: Array (Int, Int) Char -> [(Int, Int)] -> Bool
checkSequence arr ps@[_, _, _, _] =
    let (_, (maxY, maxX)) = bounds arr
        xmas =
            map
                ( \p@(x, y) ->
                    if x > maxX || x < 1 || y > maxY || y < 1
                        then
                            '\0'
                        else arr ! p
                )
                ps
     in xmas == xmasLetters
checkSequence _ _ = False

debugPrint :: Array (Int, Int) Char -> (Int, Int) -> [(Int, Int)] -> IO ()
debugPrint arr (maxY, maxX) points = debugPrint' (1, 1)
  where
    debugPrint' p@(y, x) = do
        if p `elem` points
            then putChar $ arr ! (y, x)
            else putChar '.'
        if x == maxX
            then do
                putChar '\n'
                when (y /= maxY) $ debugPrint' (y + 1, 1)
            else debugPrint' (y, x + 1)

xmasLetters :: String
xmasLetters = "XMAS"
