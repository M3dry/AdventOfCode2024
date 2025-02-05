module Day7 where

import Control.Applicative (Alternative (many))
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (char)
import Utility (Parser, intP)

type Case = (Int, [Int])

caseP :: Parser Case
caseP = (,) <$> intP <*> (char ':' *> many (char ' ' *> intP))

inputP :: Parser [Case]
inputP = many (caseP <* char '\n')

day7Part1 :: String -> Int
day7Part1 input = case parseMaybe inputP input of
    Just cases -> sum [res | c@(res, _) <- cases, checkCase [(+), (*)] c]
    Nothing -> error ""

day7Part2 :: String -> Int
day7Part2 input = case parseMaybe inputP input of
    Just cases -> sum [res | c@(res, _) <- cases, checkCase [(+), (*), con] c]
    Nothing -> error ""

type Operator = Int -> Int -> Int

con :: Int -> Int -> Int
con x y = x * b + y
  where
    b | y < 10 = 10
      | y < 100 = 100
      | y < 1000 = 1000
      | otherwise = error "con: rhs too large"

checkCase :: [Operator] -> Case -> Bool
checkCase _ (_, []) = False
checkCase ops (res, num : nums) = checkCase' num nums
  where
    checkCase' total [] = total == res
    checkCase' total (n : ns)
        | total > res = False
        | otherwise = any (\op -> checkCase' (op total n) ns) ops
