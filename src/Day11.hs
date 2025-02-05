module Day11 where

import Control.Applicative (Alternative (many))
import Data.List (unfoldr)
import Data.Map qualified as M
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (char)
import Utility (Parser, intP)

inputP :: Parser (M.Map Int Int)
inputP = ((M.fromList . map (,1)) .) . (:) <$> intP <*> many (char ' ' *> intP) <* char '\n'

day11Part1 :: String -> Int
day11Part1 input =
    let (Just stones) = parseMaybe inputP input
     in M.foldr (+) 0 $ blink 25 stones

day11Part2 :: String -> Int
day11Part2 input =
    let (Just stones) = parseMaybe inputP input
     in M.foldr (+) 0 $ blink 75 stones

blink :: Int -> M.Map Int Int -> M.Map Int Int
blink 0 stones = stones
blink n stones
    | n > 0 =
        blink (n - 1) $
            M.foldrWithKey
                ( \stoneId amount stones' ->
                    foldr (\stoneId' map' -> M.insertWith (+) stoneId' amount map') stones' $ modify stoneId
                )
                M.empty
                stones
    | otherwise = error ""

modify :: Int -> [Int]
modify 0 = [1]
modify i = case digits i of
    dgs | even $ length dgs -> let (n1, n2) = splitAt (length dgs `div` 2) dgs in [undigits n1, undigits n2]
    _ -> [i * 2024]

digits :: Int -> [Int]
digits = reverse . unfoldr (\i -> if i == 0 then Nothing else Just (i `mod` 10, i `div` 10))

undigits :: [Int] -> Int
undigits = foldl ((+) . (* 10)) 0
