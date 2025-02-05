module Day14 where

import Control.Applicative (Alternative (many), optional)
import Control.Arrow (first)
import Data.List (partition, sort)
import Debug.Trace (traceShow)
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (char, string)
import Utility (Parser, intP)

type Point = (Int, Int)

intP' :: Parser Int
intP' = do
    sign <- optional $ char '-'
    int <- intP
    case sign of
        Just _ -> return $ -int
        _ -> return int

robotP :: Parser (Point, Point)
robotP = do
    _ <- string "p="
    pos <- (,) <$> intP' <*> (char ',' *> intP')
    _ <- string " v="
    velocity <- (,) <$> intP' <*> (char ',' *> intP')
    return (pos, velocity)

inputP :: Parser [(Point, Point)]
inputP = many (robotP <* char '\n')

day14Part1 :: String -> Int
day14Part1 input =
    let Just robots = parseMaybe inputP input
        maxBounds@(maxX, maxY) = (100, 102)
        robots' = tick 100 maxBounds robots
        (left, right) = partition (\(x, _) -> x < maxX `div` 2) $ filter (\(x, y) -> x /= maxX `div` 2 && y /= maxY `div` 2) $ map fst robots'
        (topleft, bottomleft) = partition (\(_, y) -> y < maxY `div` 2) left
        (topright, bottomright) = partition (\(_, y) -> y < maxY `div` 2) right
     in length topleft * length topright * length bottomleft * length bottomright

day14Part2 :: String -> IO ()
day14Part2 input =
    let Just robots = parseMaybe inputP input
        maxBounds = (100, 102)
     in interactive maxBounds 103 $ tick 103 maxBounds robots
  where
    interactive maxBounds n robs = do
        putStrLn $ "Iteration: " ++ show n
        printRobots maxBounds $ map fst robs
        _ <- getChar
        interactive maxBounds (n + 101) $ tick 101 maxBounds robs

printRobots :: Point -> [Point] -> IO ()
printRobots (maxX, maxY) robots =
    do
        mapM_
            ( \y -> do
                mapM_
                    ( \x -> do
                        let occurs = occurences (x, y) robots
                        if occurs > 0
                            then
                                putStr $ show occurs
                            else putStr "."
                    )
                    [0 .. maxX]
                putStrLn ""
            )
            [0 .. maxY]
  where
    occurences :: (Eq a) => a -> [a] -> Int
    occurences _ [] = 0
    occurences a (x : xs)
        | a == x = 1 + occurences a xs
        | otherwise = occurences a xs

tick :: Int -> Point -> [(Point, Point)] -> [(Point, Point)]
tick 0 _ robots = robots
tick n maxBounds robots = tick (n - 1) maxBounds $ walkRobots maxBounds robots

walkRobots :: Point -> [(Point, Point)] -> [(Point, Point)]
walkRobots (maxX, maxY) = map walk
  where
    walk (pos, velocity) = first wrap (pos .+. velocity, velocity)
    wrap (x, y) =
        let x'
                | x > maxX = x - maxX - 1
                | x < 0 = maxX + x + 1
                | otherwise = x
            y'
                | y > maxY = y - maxY - 1
                | y < 0 = maxY + y + 1
                | otherwise = y
         in (x', y')

(.+.) :: Point -> Point -> Point
(y1, x1) .+. (y2, x2) = (y1 + y2, x1 + x2)
