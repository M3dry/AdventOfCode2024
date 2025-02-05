module Day17 where

import Control.Applicative (Alternative (many))
import Control.Arrow (Arrow (first))
import Data.Array (Array, bounds, listArray, (!))
import Data.Bits (shiftL, xor)
import Data.Foldable (foldl')
import Text.Megaparsec (parse)
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Char (char)
import Utility (Parser, intP)

type Registers = (Int, Int, Int)

data Instruction = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV
    deriving (Show, Enum)

execute :: Registers -> Array Int (Instruction, Int) -> Int -> (Registers, [Int])
execute (a, b, c) instructions pc
    | pc > len = ((a, b, c), [])
    | otherwise =
        let (opcode, operand) = instructions ! pc
            pc' = pc + 1
         in case opcode of
                ADV -> execute (a `div` (2 ^ comboOperand operand), b, c) instructions pc'
                BXL -> execute (a, b `xor` operand, c) instructions pc'
                BST -> execute (a, comboOperand operand `mod` 8, c) instructions pc'
                JNZ | a /= 0 -> execute (a, b, c) instructions operand
                JNZ -> execute (a, b, c) instructions pc'
                BXC -> execute (a, b `xor` c, c) instructions pc'
                OUT ->
                    let (regs, out) = execute (a, b, c) instructions pc'
                     in (regs, (comboOperand operand `mod` 8) : out)
                BDV -> execute (a, a `div` (2 ^ comboOperand operand), c) instructions pc'
                CDV -> execute (a, b, a `div` (2 ^ comboOperand operand)) instructions pc'
  where
    (_, len) = bounds instructions
    comboOperand 4 = a
    comboOperand 5 = b
    comboOperand 6 = c
    comboOperand 7 = error "invalid program"
    comboOperand l = l

inputP :: Parser (Registers, Array Int (Instruction, Int))
inputP = do
    a <- string "Register A: " *> intP <* char '\n'
    b <- string "Register B: " *> intP <* char '\n'
    c <- string "Register C: " *> intP <* char '\n'

    _ <- string "\nProgram: "

    instructions <- (:) <$> ((,) <$> intP <*> (char ',' *> intP)) <*> many ((,) <$> (char ',' *> intP) <*> (char ',' *> intP))
    let instructions' = map (first (toEnum @Instruction)) instructions
    return ((a, b, c), listArray (0, length instructions' - 1) instructions')

day17Part1 :: String -> String
day17Part1 input =
    let Right (registers, instructions) = parse inputP "" input
     in init $ tail $ show (snd $ execute registers instructions 0)

day17Part2 :: String -> Int
day17Part2 input = solveA instructions (b, c)
  where
    Right ((_, b, c), instructions) = parse inputP "" input

solveA :: Array Int (Instruction, Int) -> (Int, Int) -> Int
solveA instructions (b, c) =
    let (as, _) =
            foldl'
                ( \(as', expected) out ->
                    ([a + instr | a <- map (`shiftL` 3) as', instr <- [0 .. 7], snd (execute (a + instr, b, c) instructions 0) == out : expected], out : expected)
                )
                ([0], [])
                $ reverse instructions'
     in minimum as
  where
    instructions' = concatMap (\(opcode, operand) -> fromEnum opcode : [operand]) instructions
