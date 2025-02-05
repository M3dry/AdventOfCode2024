module Day10 where

import Data.Array (Array, assocs, listArray, (!))
import Data.Array.Base ((!?))
import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Utility (InputType (Test), readInput)

data Trail = Split Point [Trail]
    deriving (Show)

type Point = (Int, Int)

inputP :: String -> (Array Point Int, [Point])
inputP input =
    let maxBounds = (length linesI - 1, length (head linesI) - 1)
        arr = listArray ((0, 0), maxBounds) $ map digitToInt $ filter ('\n' /=) input
        zeroes = map fst $ filter ((0 ==) . snd) $ assocs arr
     in (arr, zeroes)
  where
    linesI = lines input

day10Part1 :: String -> Int
day10Part1 input = let (arr, zeroes) = inputP input in sum $ map (S.size . getPeaks arr) $ mapMaybe (pruneTrail arr . createTrail arr) zeroes

day10Part2 :: String -> Int
day10Part2 input = let (arr, zeroes) = inputP input in sum $ map (length . getRating arr) $ mapMaybe (pruneTrail arr . createTrail arr) zeroes

createTrail :: Array Point Int -> Point -> Trail
createTrail arr p = Split p $ mapMaybe (goDirection (arr ! p) . (p .+.)) [(-1, 0), (1, 0), (0, -1), (0, 1)]
  where
    goDirection height p' = case arr !? p' of
        Just next | next == height + 1 -> Just $ createTrail arr p'
        _ -> Nothing

pruneTrail :: Array Point Int -> Trail -> Maybe Trail
pruneTrail arr trail@(Split point trails)
    | reachesPeak arr trail = Just $ Split point $ mapMaybe (pruneTrail arr) trails
    | otherwise = Nothing

reachesPeak :: Array Point Int -> Trail -> Bool
reachesPeak arr (Split point trails)
    | arr ! point == 9 = True
    | otherwise = any (reachesPeak arr) trails

getPeaks :: Array Point Int -> Trail -> S.Set Point
getPeaks arr (Split point [])
  | arr ! point == 9 = S.singleton point
  | otherwise = S.empty
getPeaks arr (Split _ trails) = foldr (S.union . getPeaks arr) S.empty trails

getRating :: Array Point Int -> Trail -> [Point]
getRating arr (Split point [])
  | arr ! point == 9 = [point]
  | otherwise = []
getRating arr (Split _ trails) = concatMap (getRating arr) trails

test :: IO ()
test = do
    (arr, zeroes) <- inputP <$> readInput 10 Test
    let peaks = sum $ map (S.size . getPeaks arr) $ mapMaybe (pruneTrail arr . createTrail arr) zeroes
    print peaks
    return ()

(.+.) :: Point -> Point -> Point
(y1, x1) .+. (y2, x2) = (y1 + y2, x1 + x2)
