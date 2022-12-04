#!/usr/bin/env cabal
import Control.Arrow (first)
import Data.Char (isDigit, ord)

{- cabal:
  build-depends: base >= 4.7 && < 5
-}

main :: IO ()
main = do
  ranges <- parseInput <$> readFile "input.txt"
  print $ length $ filter (uncurry isOneOfRangesInAnother) ranges
  print $ length $ filter (uncurry doRangesOverlap) ranges

type Range = (Int, Int)

parseInput :: String -> [(Range, Range)]
parseInput = map parseLine . lines
  where
    parseLine :: String -> (Range, Range)
    parseLine line =
      let (r1, line') = parseRange line
          (r2, _) = parseRange $ drop 1 line'
       in (r1, r2)
    parseRange :: String -> (Range, String)
    parseRange str =
      let (a, str') = parseNumber str
          (b, str'') = parseNumber $ drop 1 str'
       in ((a, b), str'')

isRangeInRange :: Range -> Range -> Bool
isRangeInRange (a1, b1) (a2, b2) = a1 >= a2 && b1 <= b2

isOneOfRangesInAnother :: Range -> Range -> Bool
isOneOfRangesInAnother r1 r2 = isRangeInRange r1 r2 || isRangeInRange r2 r1

doRangesOverlap :: Range -> Range -> Bool
doRangesOverlap (a, b) (c, d) = not (c > b || d < a)

parseNumber :: String -> (Int, String)
parseNumber xs = first read $ span isDigit xs
