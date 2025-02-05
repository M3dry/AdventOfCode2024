{-# LANGUAGE LambdaCase #-}

module Day15 where

import Control.Arrow (Arrow (second))
import Data.List (elemIndex)
import Data.Map qualified as M

type Point = (Int, Int)

data Obstacle = Wall | Box | BigBoxRight | BigBoxLeft
    deriving (Show, Eq)

data Move = MUp | MDown | MLeft | MRight
    deriving (Show, Eq)

moveP :: Move -> Point
moveP MUp = (0, -1)
moveP MDown = (0, 1)
moveP MLeft = (-1, 0)
moveP MRight = (1, 0)

inputP :: String -> (Point, M.Map Point Obstacle, [Move])
inputP input =
    let (grid, moves) = split $ lines input
        (robot, gridMap) =
            foldr
                ( \((x, y), ch) (rob, m) -> case ch of
                    '@' -> ((x, y), m)
                    'O' -> (rob, M.insert (x, y) Box m)
                    '[' -> (rob, M.insert (x + 1, y) BigBoxRight $ M.insert (x, y) BigBoxLeft m)
                    '#' -> (rob, M.insert (x, y) Wall m)
                    '.' -> (rob, m)
                    ']' -> (rob, m)
                    c -> error $ "invalid char: " ++ show c
                )
                ((0, 0) :: Point, M.empty)
                $ [((x, y), ch) | (y, l) <- zip [0 ..] grid, (x, ch) <- zip [0 ..] l]
        moves' =
            map
                ( \case
                    '^' -> MUp
                    'v' -> MDown
                    '<' -> MLeft
                    '>' -> MRight
                    _ -> error "invalid move"
                )
                $ concat moves
     in (robot, gridMap, moves')
  where
    split xs = case [] `elemIndex` xs of
        Just i -> second tail $ splitAt i xs
        Nothing -> error "idk"

day15Part1 :: String -> Int
day15Part1 input =
    let (robot, grid, moves) = inputP input
        (_, grid') = moveMultiple moves robot grid
     in sum $ map ((\(x, y) -> y * 100 + x) . fst) $ filter ((== Box) . snd) $ M.toList grid'

day15Part2 :: String -> Int
day15Part2 input =
    let (robot, grid, moves) =
            inputP $
                concatMap
                    ( \case
                        '#' -> "##"
                        'O' -> "[]"
                        '.' -> ".."
                        '@' -> "@."
                        a -> [a]
                    )
                    input
        (_, grid') = moveMultiple moves robot grid
     in sum $ map ((\(x, y) -> y * 100 + x) . fst) $ filter ((==BigBoxLeft) . snd) $ M.toList grid'

printGrid :: Point -> M.Map Point Obstacle -> IO ()
printGrid robot grid = do
    let ((maxX, maxY), _) = last $ M.assocs grid
    mapM_
        ( \y -> do
            mapM_
                ( \x ->
                    if (x, y) == robot
                        then putStr "@"
                        else case (x, y) `M.lookup` grid of
                            Just Wall -> putStr "#"
                            Just Box -> putStr "O"
                            Just BigBoxLeft -> putStr "["
                            Just BigBoxRight -> putStr "]"
                            Nothing -> putStr "."
                )
                [0 .. maxX]
            putStrLn ""
        )
        [0 .. maxY]

moveMultiple :: [Move] -> Point -> M.Map Point Obstacle -> (Point, M.Map Point Obstacle)
moveMultiple [] robot grid = (robot, grid)
moveMultiple (m : ms) robot grid = uncurry (moveMultiple ms) $ movePoint m robot grid

movePoint :: Move -> Point -> M.Map Point Obstacle -> (Point, M.Map Point Obstacle)
movePoint move p grid = case p' `M.lookup` grid of
    Just Wall -> (p, grid)
    Just Box ->
        let (p2, grid') = movePoint move p' grid
         in if p2 == (p' .+. moveP move)
                then (p', M.insert p2 Box $ M.delete p' grid')
                else (p, grid)
    Just BigBoxLeft | move == MLeft -> error "invalid move"
    Just BigBoxRight | move == MRight -> error "invalid move"
    Just BigBoxLeft
        | move == MRight ->
            let (boxLeft, grid') = movePoint move (p' .+. moveP move) grid
             in if boxLeft == (p' .+. (2 *. moveP move))
                    then (p', M.insert boxLeft BigBoxRight $ M.insert (p' .+. moveP move) BigBoxLeft $ M.delete p' grid')
                    else (p, grid)
    Just BigBoxRight
        | move == MLeft ->
            let (boxLeft, grid') = movePoint move (p' .+. moveP move) grid
             in if boxLeft == (p' .+. (2 *. moveP move))
                    then (p', M.insert boxLeft BigBoxLeft $ M.insert (p' .+. moveP move) BigBoxRight $ M.delete p' grid')
                    else (p, grid)
    Just BigBoxRight ->
        let (boxRight, grid') = movePoint move p' grid
            (boxLeft, grid'') = movePoint move (p' .+. (-1, 0)) grid'
         in if boxRight == (boxLeft .+. (1, 0)) && boxRight == (p' .+. moveP move)
                then (p', M.insert boxLeft BigBoxLeft $ M.insert boxRight BigBoxRight $ M.delete (p' .+. (-1, 0)) $ M.delete p' grid'')
                else (p, grid)
    Just BigBoxLeft ->
        let (boxLeft, grid') = movePoint move p' grid
            (boxRight, grid'') = movePoint move (p' .+. (1, 0)) grid'
         in if boxLeft == (boxRight .+. (-1, 0)) && boxLeft == (p' .+. moveP move)
                then (p', M.insert boxLeft BigBoxLeft $ M.insert boxRight BigBoxRight $ M.delete (p' .+. (1, 0)) $ M.delete p' grid'')
                else (p, grid)
    Nothing -> (p', grid)
  where
    p' = p .+. moveP move

(.+.) :: Point -> Point -> Point
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)

(*.) :: Int -> Point -> Point
c *. (x, y) = (c * x, c * y)
