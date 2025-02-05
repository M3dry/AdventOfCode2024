module Day18 where

import Algorithm.Search (bfs)
import Control.Applicative (Alternative (many))
import Data.Bifunctor (bimap)
import Data.Set qualified as S
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (char)
import Utility (Parser, intP)

type Point = (Int, Int)

inputP :: Parser [Point]
inputP = many ((,) <$> (intP <* char ',') <*> (intP <* char '\n'))

day18Part1 :: String -> Int
day18Part1 input =
    let (Just shortest) =
            bfs
                ( \(x, y) ->
                    filter (\(x', y') -> x' >= 0 && y' >= 0 && x' <= 70 && y' <= 70 && not ((x', y') `S.member` unsafe')) $
                        map (bimap (x +) (y +)) [(1, 0), (-1, 0), (0, 1), (0, -1)]
                )
                (== (70, 70))
                (0 :: Int, 0 :: Int)
     in length shortest
  where
    Right unsafe = parse inputP "" input
    unsafe' = S.fromList $ take 1024 unsafe

day18Part2 :: String -> Point
day18Part2 input = binSearch 1 $ length unsafe
  where
    Right unsafe = parse inputP "" input

    test n =
        let unsafe' = S.fromList $ take n unsafe
         in bfs
                ( \(x, y) ->
                    filter (\(x', y') -> x' >= 0 && y' >= 0 && x' <= 70 && y' <= 70 && not ((x', y') `S.member` unsafe')) $
                        map (bimap (x +) (y +)) [(1, 0), (-1, 0), (0, 1), (0, -1)]
                )
                (== (70, 70))
                (0 :: Int, 0 :: Int)

    binSearch lower upper =
        let middle = (upper - lower) `div` 2 + lower
         in case test middle of
             Just _ | test (middle + 1) == Nothing -> unsafe !! middle
             Just _ -> binSearch middle upper
             Nothing -> binSearch lower middle
