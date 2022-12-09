{-# LANGUAGE BangPatterns #-}

module Day8b where

import Control.Arrow (first, second)
import Data.Char (digitToInt)
import Data.List (foldl', group, sort)
import qualified Data.Vector as V
import Paths_aoc2022 (getDataFileName)

day8b :: IO ()
day8b = do
  matrix <- parseInput . lines <$> (getDataFileName "input8.txt" >>= readFile)
  let trees = [evalTree matrix (r, c) | r <- [0 .. V.length matrix - 1], c <- [0 .. V.length (V.head matrix) - 1]]
  print $ length $ filter (== True) $ snd <$> trees
  print $ maximum $ fst <$> trees

type Height = Int

type Matrix a = V.Vector (V.Vector a)

type Loc = (Int, Int) -- row, col (0 index)

parseInput :: [String] -> Matrix Height
parseInput = V.fromList . map (V.fromList . map digitToInt)

view :: Matrix Height -> Height -> (Loc -> Loc) -> Loc -> (Int, Bool) -- (viewing distance, is visible from edge)
view !m !viewpointHeight !next (!r, !c)
  | not (0 <= r' && r' < V.length m && 0 <= c' && c' < V.length (V.head m)) = (0, True)
  | viewpointHeight <= m V.! r' V.! c' = (1, False)
  | otherwise = first (1 +) $ view m viewpointHeight next (r', c')
  where
    (r', c') = next (r, c)

evalTree :: Matrix Height -> Loc -> (Int, Bool) -- (scenic score, is visible)
evalTree m (r, c) =
  let views = [view m (m V.! r V.! c) next (r, c) | next <- [left, right, up, down]]
   in (product $ fst <$> views, or $ snd <$> views)
  where
    (left, right, up, down) = (second (subtract 1), second (+ 1), first (subtract 1), first (+ 1))
