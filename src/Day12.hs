module Day12 where

import Data.List (groupBy, sortBy, unfoldr)
import Data.Map qualified as M
import Data.Set qualified as S

type Point = (Int, Int)

inputP :: String -> M.Map Char (S.Set Point)
inputP input = foldr (\(p, ch) m -> M.insertWith S.union ch (S.singleton p) m) M.empty $ [((y, x), ch) | (y, l) <- zip [0 ..] (lines input), (x, ch) <- zip [0 ..] l, ch /= '\n']

day12Part1 :: String -> Int
day12Part1 input =
    let regions = M.foldr (\set regions' -> connected set ++ regions') [] $ inputP input
        areas = map S.size regions
        perimeters = map perimeter regions
     in sum $ zipWith (*) areas perimeters

day12Part2 :: String -> Int
day12Part2 input =
    let regions = M.foldr (\set regions' -> connected set ++ regions') [] $ inputP input
        areas = map S.size regions
        wallss = map walls regions
     in sum $ zipWith (*) areas wallss

perimeter :: S.Set Point -> Int
perimeter points
    | size == 0 = 0
    | size == 1 = 4
    | otherwise = S.foldr (\p peri -> peri + sum (map (\dir -> fromEnum $ not $ (p .+. dir) `S.member` points) [(1, 0), (-1, 0), (0, 1), (0, -1)])) 0 points
  where
    size = S.size points

data Face = FUp | FDown | FLeft | FRight
    deriving (Show, Eq, Ord)

walls :: S.Set Point -> Int
walls points
    | size == 0 = 0
    | size == 1 = 4
    | otherwise =
        let sidePieces =
                S.toList $
                    S.foldr
                        ( \p s ->
                            let faces = map fst $ filter snd $ map (\(dir, dirTuple) -> (dir, not $ (p .+. dirTuple) `S.member` points)) [(FDown, (1, 0)), (FUp, (-1, 0)), (FRight, (0, 1)), (FLeft, (0, -1))]
                             in foldr (\face s' -> S.insert (p, face) s') s faces
                        )
                        S.empty
                        points
            grouped = (map (groupBy f) $ groupBy (\(_, f1) (_, f2) -> f1 == f2) $ sortBy cmpF sidePieces)
         in sum $ map (sum . map ((\ (ps, fs) -> splits (head fs) ps) . unzip)) grouped
  where
    size = S.size points

    splits :: Face -> [Point] -> Int
    splits face (p : p2 : ps) =
        let move = if face == FUp || face == FDown then (0, 1) else (1, 0)
         in if p .+. move /= p2 then 1 + splits face (p2:ps) else splits face (p2:ps)
    splits _ _ = 1

    f ((y1, _), FUp) ((y2, _), FUp) = y1 == y2
    f ((y1, _), FDown) ((y2, _), FDown) = y1 == y2
    f ((_, x1), FLeft) ((_, x2), FLeft) = x1 == x2
    f ((_, x1), FRight) ((_, x2), FRight) = x1 == x2
    f _ _ = error "kys"

    cmpF ((y1, _), FUp) ((y2, _), f2) = compare FUp f2 <> compare y1 y2
    cmpF ((y1, _), FDown) ((y2, _), f2) = compare FDown f2 <> compare y1 y2
    cmpF ((_, x1), FLeft) ((_, x2), f2) = compare FLeft f2 <> compare x1 x2
    cmpF ((_, x1), FRight) ((_, x2), f2) = compare FRight f2 <> compare x1 x2

connected :: S.Set Point -> [S.Set Point]
connected points
    | S.null points = []
    | otherwise = unfoldr f points
  where
    f :: S.Set Point -> Maybe (S.Set Point, S.Set Point)
    f possible
        | S.null possible = Nothing
        | otherwise = Just $ connect (S.elemAt 0 possible) possible

connect :: Point -> S.Set Point -> (S.Set Point, S.Set Point)
connect p points
    | p `S.member` points =
        foldr
            (\dir (ps, possible) -> let (ps', possible') = connect (p .+. dir) possible in (ps `S.union` ps', possible'))
            (S.singleton p, p `S.delete` points)
            [(1, 0), (-1, 0), (0, 1), (0, -1)]
    | otherwise = (S.empty, points)

(.+.) :: Point -> Point -> Point
(y1, x1) .+. (y2, x2) = (y1 + y2, x1 + x2)
