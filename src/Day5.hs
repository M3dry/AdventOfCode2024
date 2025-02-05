module Day5 where

import Data.List (elemIndex, (\\))
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Utility (Parser, intP)

data Rule = Rule Int Int
  deriving (Show)

type Row = [Int]

ruleP :: Parser Rule
ruleP = Rule <$> intP <* char '|' <*> intP

printRowP :: Parser Row
printRowP = do
  n <- intP
  ns <- many (char ',' *> intP)
  return (n : ns)

inputP :: Parser ([Rule], [Row])
inputP = do
  rules <- some (ruleP <* char '\n')
  _ <- char '\n'

  rows <- some (printRowP <* char '\n')
  return (rules, rows)

parseInput :: String -> Maybe ([Rule], [Row])
parseInput = parseMaybe inputP

day5Part1 :: String -> Int
day5Part1 input = case parseInput input of
  Just (rules, rows) ->
    let correctRows = filter (\row -> all (checkRow row) rules) rows
     in sum $ map middleElem correctRows
  Nothing -> error ""

day5Part2 :: String -> Int
day5Part2 input = case parseInput input of
  Just (rules, rows) ->
    let badRows = filter (\row -> not $ all (checkRow row) rules) rows
        fixedRows = map (`sortRow` rules) badRows
     in sum $ map middleElem fixedRows
  Nothing -> error ""

sortRow :: Row -> [Rule] -> Row
sortRow [] _ = []
sortRow (x : xs) rules = sortRow beforeArray rules ++ [x] ++ sortRow afterArray rules
  where
    afterArray = xs \\ before
    beforeArray = xs \\ afterArray
    before = [p | Rule p n <- rules, n == x]

middleElem :: [a] -> a
middleElem as = head $ drop (len `div` 2) as
  where
    len = length as

checkRow :: Row -> Rule -> Bool
checkRow row (Rule r1 r2) = case (r1IdxM, r2IdxM) of
  (Just r1Idx, Just r2Idx) | r1Idx > r2Idx -> False
  _ -> True
  where
    r1IdxM = r1 `elemIndex` row
    r2IdxM = r2 `elemIndex` row
