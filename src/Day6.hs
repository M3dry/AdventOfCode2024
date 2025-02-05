module Day6 where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Set qualified as S
import Debug.Trace (traceShow)

type Point = (Int, Int)

type Grid = S.Set Point

type MaxBounds = (Int, Int)

data Facing = FUp | FDown | FLeft | FRight
    deriving (Show, Eq, Ord)

movementPoint :: Facing -> (Int, Int)
movementPoint FUp = (-1, 0)
movementPoint FDown = (1, 0)
movementPoint FLeft = (0, -1)
movementPoint FRight = (0, 1)

facingNext :: Facing -> Facing
facingNext FUp = FRight
facingNext FRight = FDown
facingNext FDown = FLeft
facingNext FLeft = FUp

parseGrid :: String -> (Grid, MaxBounds, Point)
parseGrid input =
    let height = length $ lines input
        width = length $ head $ lines input
        dims = (height, width)
        (obstacles, Just point, _) =
            foldl
                ( \(g, p, i) ch ->
                    case ch of
                        '#' -> (S.insert (idxTo2DIdx dims i) g, p, i + 1)
                        '^' -> (g, Just $ idxTo2DIdx dims i, i + 1)
                        _ -> (g, p, i + 1)
                )
                (S.empty, Nothing, 0)
                $ filter (/= '\n') input
     in (obstacles, dims, point)
  where
    idxTo2DIdx :: (Int, Int) -> Int -> (Int, Int)
    idxTo2DIdx (height, width) i = (i `div` height, i `mod` width)

day6Part1 :: String -> Int
day6Part1 input =
    let (grid, maxBounds, guardPos) = parseGrid input
     in getUniquePoints grid maxBounds guardPos FUp

day6Part2 :: String -> Int
day6Part2 input =
    let (grid, (maxY, maxX), guardPos) = parseGrid input
     in sum $
            [ traceShow (y, x) $ fromEnum $ isLoop (S.insert (y, x) grid) (maxY, maxX) guardPos FUp
            | y <- [0 .. maxY - 1]
            , x <- [0 .. maxX - 1]
            ]

getUniquePoints :: Grid -> MaxBounds -> Point -> Facing -> Int
getUniquePoints grid (maxY, maxX) guard = S.size . getUniquePoints' (S.singleton guard) guard
  where
    getUniquePoints' uniquePoints pos facing = case walkGuard grid pos facing of
        ((nY, nX), _) | nY >= maxY || nX >= maxX || nY < 0 || nX < 0 -> uniquePoints
        (newPos, newFacing) -> getUniquePoints' (S.insert newPos uniquePoints) newPos newFacing

isLoop :: Grid -> MaxBounds -> Point -> Facing -> Bool
isLoop grid (maxY, maxX) guard facing = isLoop' (S.singleton (guard, facing)) guard facing
  where
    isLoop' uniquePoints pos facing' = case walkGuard grid pos facing' of
        ((nY, nX), _) | nY >= maxY || nX >= maxX || nY < 0 || nX < 0 -> False
        point | point `S.member` uniquePoints -> True
        (newPos, newFacing) -> isLoop' (S.insert (newPos, newFacing) uniquePoints) newPos newFacing

walkGuard :: Grid -> Point -> Facing -> (Point, Facing)
walkGuard grid (gY, gX) facing
    | nextPos `S.member` grid = ((gY, gX), facingNext facing)
    | otherwise = (nextPos, facing)
  where
    nextPos = bimap (+ gY) (+ gX) $ movementPoint facing
