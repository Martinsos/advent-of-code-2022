{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Day8 where

import Data.Char (digitToInt)
import Data.List (foldl', group, sort)
import Paths_aoc2022 (getDataFileName)

day8 :: IO ()
day8 = do
  inputLines <- lines <$> (getDataFileName "input8.txt" >>= readFile)
  let matrix = (map . map) digitToInt inputLines
  print $ length $ findVisibleTrees matrix

type Matrix a = [[a]]

rotateMatrixCCW :: Matrix a -> Matrix a
rotateMatrixCCW [] = []
rotateMatrixCCW m = let (c0, m') = popFirstColumn m in rotateMatrixCCW m' ++ [c0]

rotateLocCW :: Int -> Loc -> Loc
rotateLocCW numMatrixRows (r, c) = (c, numMatrixRows - 1 - r)

popFirstColumn :: Matrix a -> ([a], Matrix a)
popFirstColumn [] = ([], [])
popFirstColumn m@([a] : _) = (map head m, [])
popFirstColumn ([] : _) = error "invalid matrix"
popFirstColumn ((r0h : r0t) : rs) = let (c0t, m') = popFirstColumn rs in (r0h : c0t, r0t : m')

type Height = Int

type Idx = Int

findTreesInRowVisibleFromLeft :: [Height] -> [Idx]
findTreesInRowVisibleFromLeft =
  fst . foldl' (\(!idxs, !maxTree) (idx, t) -> (idxs ++ [idx | t > maxTree], max maxTree t)) ([], -1) . zip [0 ..]

type Loc = (Idx, Idx) -- Row, Column

findTreesInMatrixVisibleFromLeft :: Matrix Height -> [Loc]
findTreesInMatrixVisibleFromLeft =
  foldl' (\locs (idx, r) -> locs ++ ((idx,) <$> findTreesInRowVisibleFromLeft r)) [] . zip [0 ..]

findVisibleTrees :: Matrix Height -> [Loc]
findVisibleTrees m = nubOrd $ concat [rotateAndFindTrees numRotations | numRotations <- [0 .. 3]]
  where
    rotateAndFindTrees n = applyTimes n (rotateLocCW numMatrixRows) <$> findTreesInMatrixVisibleFromLeft (applyTimes n rotateMatrixCCW m)
    applyTimes n f = (!! n) . iterate f
    numMatrixRows = length m
    nubOrd :: (Ord a) => [a] -> [a]
    nubOrd = map head . group . sort
