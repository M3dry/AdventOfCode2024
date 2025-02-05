{-# LANGUAGE ParallelListComp #-}

module Day19 where

import Control.Applicative (Alternative (many))
import Data.Array ((!), listArray)
import Data.List (stripPrefix, tails)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (char, lowerChar)
import Utility (Parser)

inputP :: Parser ([String], [String])
inputP = do
  available <- (:) <$> many lowerChar <*> many (char ',' *> char ' ' *> many lowerChar)
  _ <- char '\n' *> char '\n'
  needed <- many (many lowerChar <* char '\n')
  return (available, needed)

day19Part1 :: String -> Int
day19Part1 input = sum $ map (fromEnum . valid available) needed
  where
    Right (available, needed) = parse inputP "" input

day19Part2 :: String -> Int
day19Part2 input = sum $ map (combinations (foldMap toTrie available)) needed
  where
    Right (available, needed) = parse inputP "" input

valid :: [String] -> String -> Bool
valid _ [] = True
valid available str = any (valid available) $ mapMaybe (`stripPrefix` str) $ mapMaybe (\a -> if take (length a) str == a then Just a else Nothing) available

combinations :: Trie -> String -> Int
combinations trie str = memo ! 0
  where
    n = length str
    memo = listArray (0, n) [if i == n then 1 else sum [memo ! j | j <- matches trie i suffix] | i <- [0 .. n] | suffix <- tails str]

data Trie = Node !Bool (M.Map Char Trie)

toTrie :: String -> Trie
toTrie = foldr (\x t -> Node False (M.singleton x t)) (Node True M.empty)

matches :: Trie -> Int -> String -> [Int]
matches (Node b xs) n yys =
  [n | b] ++
  case yys of
    y:ys | Just t <- M.lookup y xs -> matches t (n+1) ys
    _ -> []

instance Semigroup Trie where
  Node x xs <> Node y ys = Node (x || y) (M.unionWith (<>) xs ys)

instance Monoid Trie where
  mempty = Node False M.empty
