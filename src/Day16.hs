module Day16 where

import Algorithm.Search (dijkstra)
import Data.Maybe (mapMaybe)
import Data.Set qualified as S

type Point = (Int, Int)

data Path a = Path a [Path a]
    deriving (Show)

data Direction = East | South | West | North
    deriving (Show, Eq, Enum, Ord)

moveP :: Direction -> Point
moveP East = (1, 0)
moveP West = (-1, 0)
moveP South = (0, 1)
moveP North = (0, -1)

getDirection :: Point -> Point -> Direction
getDirection p1 p2 = case p2 .-. p1 of
    (1, 0) -> East
    (-1, 0) -> West
    (0, 1) -> South
    (0, -1) -> North
    p -> error $ "Invalid direction: " ++ show p

inputP :: String -> (S.Set Point, Point, Point)
inputP input = (S.fromList $ map fst $ filter ((== '.') . snd) input', fst $ head $ filter ((== 'S') . snd) input', fst $ head $ filter ((== 'E') . snd) input')
  where
    input' = [((x, y), ch) | (y, l) <- zip [0 ..] $ lines input, (x, ch) <- zip [0 ..] l]

day16Part1 :: String -> Int
day16Part1 input =
    let (mazeMap, start, end) = inputP input
        Just (cost, _) =
            dijkstra
                ( \(_, p) ->
                    [ (dir, p')
                    | (dir, p') <- map (\d -> (d, p .+. moveP d)) [East, South, West, North]
                    , p' `S.member` mazeMap || p' == end
                    ]
                )
                (\(d1, p1) (d2, p2) -> (if p1 == p2 then 0 else 1) + (if d1 /= d2 then 1000 else 0))
                ((== end) . snd)
                (East, start)
     in cost

smallestSum :: Path Int -> Int
smallestSum (Path n []) = n
smallestSum (Path n paths) = n + minimum (map smallestSum paths)

score :: Direction -> Path Point -> Path Int
score _ (Path _ []) = Path 0 []
score direction (Path p1 paths) =
    Path 0 $
        map
            ( \path@(Path p2 _) ->
                let newDirection = getDirection p1 p2
                    (Path n paths') = score newDirection path
                 in if newDirection == direction
                        then
                            Path (n + 1) paths'
                        else Path (n + 1 + 1000) paths'
            )
            paths

getAllPaths :: S.Set Point -> Point -> [Point] -> Path Point
getAllPaths _ end [] = Path end []
getAllPaths mazeMap end stack =
    let possibleMoves = [p .+. dir | p <- stack, dir <- map moveP [East, South, West, North], (p .+. dir) `S.member` mazeMap || (p .+. dir) == end]
     in undefined

(.+.) :: Point -> Point -> Point
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)

(.-.) :: Point -> Point -> Point
(x1, y1) .-. (x2, y2) = (x1 - x2, y1 - y2)
