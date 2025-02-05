module Utility where

import System.FilePath
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar)
import Data.List (group, sort)

data InputType = Test | Real | Custom String

instance Show InputType where
    show Test = "test"
    show Real = "real"
    show (Custom s) = s

readInput :: Int -> InputType -> IO String
readInput day input_type = do
    readFile $ "inputs" </> ("day" ++ show day) </> show input_type

type Parser = Parsec Void String

intP :: Parser Int
intP = read @Int <$> many digitChar

dedup :: (Ord a) => [a] -> [a]
dedup = map head . group . sort
