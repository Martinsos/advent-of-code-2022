#!/usr/bin/env cabal

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Arrow (first)
import Data.Char (isDigit, ord)
import qualified Data.HashMap.Strict as M
import Data.List (foldl', isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

{- cabal:
  build-depends: base >= 4.7 && < 5, split, unordered-containers ^>= 0.2.19.1
-}

main :: IO ()
main = do
  (crateStacks, moves) <- parseInput . lines <$> readFile "input.txt"
  print $ map head $ M.elems $ foldl' moveCrates9000 crateStacks moves
  print $ map head $ M.elems $ foldl' moveCrates9001 crateStacks moves

type CrateStacks = M.HashMap Int [Crate]

type Crate = Char

type Move = (Int, Int, Int) -- (amount, src, dst)

parseInput :: [String] -> (CrateStacks, [Move])
parseInput lines =
  let (crateStacks, lines') =
        first (crateLinesToCrateStacks . map parseCratesLine) $ break isIdxsLine lines
      moves = parseMoveLine <$> drop 2 lines'
   in (crateStacks, moves)

parseCratesLine :: String -> [Maybe Crate]
parseCratesLine (' ' : ' ' : ' ' : rest) = Nothing : parseCratesLine (drop 1 rest)
parseCratesLine ('[' : c : ']' : rest) = Just c : parseCratesLine (drop 1 rest)
parseCratesLine [] = []
parseCratesLine _ = error "invalid crates line"

isIdxsLine :: String -> Bool
isIdxsLine line = " 1" `isPrefixOf` line

parseMoveLine :: String -> Move
parseMoveLine line =
  case splitOn " " line of
    ["move", amount, "from", src, "to", dst] -> (read amount, read src, read dst)
    _ -> error "invalid move line"

crateLinesToCrateStacks :: [[Maybe Crate]] -> CrateStacks
crateLinesToCrateStacks = foldl' addCrateLineToCrateStacks M.empty . reverse

addCrateLineToCrateStacks :: CrateStacks -> [Maybe Crate] -> CrateStacks
addCrateLineToCrateStacks initialStacks crates =
  foldl' addCrateToStacks initialStacks $ zip [1 ..] crates
  where
    addCrateToStacks :: CrateStacks -> (Int, Maybe Crate) -> CrateStacks
    addCrateToStacks stacks (idx, crate) =
      case crate of
        Just c -> M.insertWith (++) idx [c] stacks
        Nothing -> stacks

moveCratesTogether :: Move -> CrateStacks -> CrateStacks
moveCratesTogether (amount, src, dst) stacks =
  let crates = take amount $ fromJust $ M.lookup src stacks
      stacks' = M.adjust (drop amount) src stacks
      stacks'' = M.insertWith (++) dst crates stacks'
   in stacks''

moveCrates9000 :: CrateStacks -> Move -> CrateStacks
moveCrates9000 stacks (0, _, _) = stacks
moveCrates9000 stacks (n, src, dst) =
  moveCrates9000 (moveCratesTogether (1, src, dst) stacks) (n - 1, src, dst)

moveCrates9001 :: CrateStacks -> Move -> CrateStacks
moveCrates9001 stacks (n, src, dst) = moveCratesTogether (n, src, dst) stacks
