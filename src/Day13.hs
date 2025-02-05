module Day13 where

import Control.Applicative (Alternative (many))
import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.Ratio (denominator, numerator)
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (char, string)
import Utility (Parser, intP)

type Point = (Integer, Integer)

prizeP :: Parser (Point, Point, Point)
prizeP = do
    a <- on (,) toInteger <$> (string "Button A: X+" *> intP) <*> (string ", Y+" *> intP <* char '\n')
    b <- on (,) toInteger <$> (string "Button B: X+" *> intP) <*> (string ", Y+" *> intP <* char '\n')
    p <- on (,) toInteger <$> (string "Prize: X=" *> intP) <*> (string ", Y=" *> intP <* char '\n')
    return (a, b, p)

inputP :: Parser [(Point, Point, Point)]
inputP = (:) <$> prizeP <*> many (char '\n' *> prizeP)

day13Part1 :: String -> Integer
day13Part1 input =
    let Just prizes = parseMaybe inputP input
     in sum $ map (\(x, y) -> 3 * x + y) $ filter (\(x, y) -> x < 100 && y < 100) $ mapMaybe (bestMove 0) prizes

day13Part2 :: String -> Integer
day13Part2 input =
    let Just prizes = parseMaybe inputP input
     in sum $ map (\(x, y) -> 3 * x + y) $ mapMaybe (bestMove 10000000000000) prizes

bestMove :: Integer -> (Point, Point, Point) -> Maybe (Integer, Integer)
bestMove incr ((ax, ay), (bx, by), p) = do
    let mR = toRational (ay * px - ax * py) / toRational (ay * bx - ax * by)
        (mN, mD) = (numerator mR, denominator mR)
    _ <- guard $ mN `mod` mD == 0
    let m = toInteger $ mN `div` mD
        n = toRational (toInteger px - m * toInteger bx) / toRational ax
        n' = toRational (toInteger py - m * toInteger by) / toRational ay
        (nN, nD) = (numerator n, denominator n)
    _ <- guard $ n == n'
    _ <- guard $ nN `mod` nD == 0
    return (toInteger $ nN `div` nD, m)
  where
    (px, py) = bimap (incr +) (incr +) p
