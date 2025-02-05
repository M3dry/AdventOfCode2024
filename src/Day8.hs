module Day8 where

import Data.List (foldl', unfoldr)
import Data.Map qualified as M
import Data.Set qualified as S

type Point = (Int, Int)

inputP :: String -> (M.Map Char (S.Set Point), Point)
inputP input =
    let towers =
            foldl'
                ( \m (p, ch) ->
                    if ch /= '.'
                        then M.insertWith S.union ch (S.singleton p) m
                        else m
                )
                M.empty
                input'
     in (towers, (1, 1) .+. maximum (map fst input'))
  where
    input' = [((y, x), ch) | (y, l) <- zip [0 ..] (lines input), (x, ch) <- zip [0 ..] l]

day8Part1 :: String -> Int
day8Part1 input =
    let (points, (maxY, maxX)) = inputP input
        antinodes = M.foldl' (\acc ps -> acc `S.union` allAntinodes (S.toList ps)) S.empty points
     in S.size $ S.filter (\(y, x) -> x >= 0 && y >= 0 && x < maxX && y < maxY) antinodes

day8Part2 :: String -> Int
day8Part2 input =
    let (points, maxBounds) = inputP input
        antinodes = M.foldl' (\s ps -> s `S.union` allAntinodes' maxBounds (S.toList ps)) S.empty points
     in S.size antinodes

allAntinodes :: [Point] -> S.Set Point
allAntinodes towers = foldl' (\s ps -> let (a1, a2) = createAntinode ps in S.insert a2 $ S.insert a1 s) S.empty pairs
  where
    pairs = [(x, y) | x <- towers, y <- towers, x /= y]

createAntinode :: (Point, Point) -> (Point, Point)
createAntinode (t1, t2) = (2 *. t1 .-. t2, 2 *. t2 .-. t1)

allAntinodes' :: Point -> [Point] -> S.Set Point
allAntinodes' bounds towers = foldl' (\s -> foldl' (flip S.insert) s . createAntinode' bounds) S.empty pairs
  where
    pairs = [(x, y) | x <- towers, y <- towers, x /= y]

createAntinode' :: Point -> (Point, Point) -> S.Set Point
createAntinode' (maxY, maxX) (t1, t2) =
    S.fromList $
        unfoldr (\p -> if inBounds p then Just (p, p .+. step) else Nothing) t1
        ++ unfoldr (\p -> if inBounds p then Just (p, p .-. step) else Nothing) t2
  where
    step = t1 .-. t2
    inBounds (y, x) = x >= 0 && y >= 0 && x < maxX && y < maxY

(.-.) :: Point -> Point -> Point
(x1, y1) .-. (x2, y2) = (x1 - x2, y1 - y2)

(*.) :: Int -> Point -> Point
c *. (x, y) = (c * x, c * y)

(.+.) :: Point -> Point -> Point
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)
