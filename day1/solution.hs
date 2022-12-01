#!/usr/bin/env cabal
{- cabal:
  build-depends: base >= 4.7 && < 5, split
-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ solve input
  print $ solve2 input

solve :: String -> Int
solve = maximum . caloriesPerElf

solve2 :: String -> Int
solve2 = sum . take 3 . caloriesPerElf

caloriesPerElf :: String -> [Int]
caloriesPerElf = fmap sum . (fmap . fmap) read . splitOn [""] . lines
