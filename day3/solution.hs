#!/usr/bin/env cabal
import Data.Char (ord)
import qualified Data.Set as S

{- cabal:
  build-depends: base >= 4.7 && < 5, containers ^>= 0.6.5.1
-}

main :: IO ()
main = do
  rucksacks <- lines <$> readFile "input.txt"
  print $ sum $ getItemPriority . findRucksacksWrongItem <$> rucksacks -- 8123
  print $ sum $ getItemPriority . findGroupsBadge <$> groupBy3 rucksacks -- 2620

findGroupsBadge :: (String, String, String) -> Char
findGroupsBadge = findTheSharedElem . tripleToList

findRucksacksWrongItem :: String -> Char
findRucksacksWrongItem = findTheSharedElem . pairToList . splitInHalf

findTheSharedElem :: (Ord a) => [[a]] -> a
findTheSharedElem xss = head $ S.toList $ foldr1 S.intersection $ S.fromList <$> xss

splitInHalf :: [a] -> ([a], [a])
splitInHalf xs = splitAt (length xs `div` 2) xs

getItemPriority :: Char -> Int
getItemPriority c
  | c >= 'a' && c <= 'z' = ord c - ord 'a' + 1
  | c >= 'A' && c <= 'Z' = ord c - ord 'A' + 27
  | otherwise = error "priority not defined"

groupBy3 :: [a] -> [(a, a, a)]
groupBy3 (x1 : x2 : x3 : xs) = (x1, x2, x3) : groupBy3 xs
groupBy3 [] = []
groupBy3 _ = error "not groupable by 3!"

pairToList :: (a, a) -> [a]
pairToList (a, b) = [a, b]

tripleToList :: (a, a, a) -> [a]
tripleToList (a, b, c) = [a, b, c]
