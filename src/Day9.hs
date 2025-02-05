module Day9 where

import Control.Monad.ST (ST, runST)
import Data.Array
import Data.Array.ST (readArray, writeArray)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (digitToInt)
import Data.List (groupBy, partition, sort)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import GHC.Arr (STArray, freezeSTArray, thawSTArray)

inputP :: String -> [Int]
inputP = map digitToInt . filter ('\n' /=)

data Cell
    = File Int
    | Empty
    deriving (Eq)

instance Show Cell where
    show (File id) = show id
    show Empty = "."

day9Part1 :: String -> Int
day9Part1 input =
    let diskMap = moveToFront $ expandDiskMap $ inputP input
     in foldl
            ( \sum (i, cell) ->
                sum + case cell of
                    File id -> id * i
                    Empty -> 0
            )
            0
            $ assocs diskMap

day9Part2 :: String -> Int
day9Part2 input =
    let diskMap = moveToFrontWhole $ expandDiskMap $ inputP input
     in foldl
            ( \sum (i, cell) ->
                sum + case cell of
                    File id -> id * i
                    Empty -> 0
            )
            0
            $ assocs diskMap

expandDiskMap :: [Int] -> Array Int Cell
expandDiskMap diskMap = let res = expand [0 ..] diskMap in listArray (0, length res - 1) res
  where
    -- TODO: go from right to left for more efficient concatanation
    expand (i : _) [x] = replicate x (File i)
    expand (i : is) (x : y : xs) = replicate x (File i) ++ replicate y Empty ++ expand is xs
    expand _ _ = []

-- Very imperative, idc, was a fun experience
moveToFront :: Array Int Cell -> Array Int Cell
moveToFront diskMap = runST $ do
    arr <- thawSTArray diskMap
    emptyRef <- newSTRef 0

    mapM_
        ( \i -> do
            findNextEmpty emptyRef arr
            emptyI <- readSTRef emptyRef
            if emptyI >= i
                then return ()
                else swap emptyI i arr
        )
        [len, len - 1 .. 0]

    freezeSTArray arr
  where
    (_, len) = bounds diskMap
    findNextEmpty :: STRef s Int -> STArray s Int Cell -> ST s ()
    findNextEmpty ref arr = do
        i <- readSTRef ref
        x <- readArray arr i

        if x == Empty || i == len
            then return ()
            else do
                writeSTRef ref (i + 1)
                findNextEmpty ref arr

moveToFrontWhole :: Array Int Cell -> Array Int Cell
moveToFrontWhole diskMap = runST $ do
    arr <- thawSTArray diskMap
    emptyChunksRef <- newSTRef emptyChunks

    mapM_
        ( \(from, to) -> do
            let len = to - from
            emptyChunks' <- readSTRef emptyChunksRef

            case sort $ filter (\(from', to') -> (to' - from') >= len) emptyChunks' of
                [] -> return ()
                (x : _) | x > (from, to) -> return ()
                (x@(from', to') : _) | (to' - from') > len -> do
                    writeSTRef emptyChunksRef $ (from, to) : (from' + len + 1, to') : filter (x/=) emptyChunks'
                    swapChunks (from, to) x arr
                (x : _) -> do
                    writeSTRef emptyChunksRef $ (from, to) : filter (x/=) emptyChunks'
                    swapChunks (from, to) x arr
        )
        $ reverse fileChunks

    freezeSTArray arr
  where
    (emptyChunks, fileChunks) = bimap onlyRange onlyRange $ partition ((== Empty) . snd . head) $ groupBy (\(_, a) (_, b) -> a == b) $ assocs diskMap
    onlyRange = map (\cs -> (fst $ head cs, fst $ last cs))
    swapChunks (from, to) (from', to') arr = mapM_ (\(i, j) -> swap i j arr) $ zip [from .. to] [from' .. to']

swap :: Int -> Int -> STArray s Int Cell -> ST s ()
swap i j arr = do
    x <- readArray arr i
    y <- readArray arr j
    writeArray arr i y
    writeArray arr j x
