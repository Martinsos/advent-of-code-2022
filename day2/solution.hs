#!/usr/bin/env cabal
import Data.List (find)
import Data.Maybe (fromJust)

{- cabal:
  build-depends: base >= 4.7 && < 5
-}

data Hand = Rock | Paper | Scissors
  deriving (Eq, Show)

type Round = (Hand, Hand)

main :: IO ()
main = do
  readFile "input.txt" >>= print . sum . (scoreRound <$>) . parseRounds
  readFile "input.txt" >>= print . sum . (scoreRound <$>) . parseRounds2

parseRounds :: String -> [Round]
parseRounds = map parseRound . lines
  where
    parseRound [a, ' ', b] = (parseHand a, parseHand b)
    parseRound _ = error "invalid round"

    parseHand h
      | h `elem` ['A', 'X'] = Rock
      | h `elem` ['B', 'Y'] = Paper
      | h `elem` ['C', 'Z'] = Scissors
      | otherwise = error "invalid hand"

parseRounds2 :: String -> [Round]
parseRounds2 = map parseRound . lines
  where
    parseRound [a, ' ', b] = (h1, h2)
      where
        h1 = parseHand a
        h2 = case b of
          'X' -> weakerHand h1
          'Y' -> h1
          'Z' -> strongerHand h1
          _ -> error "invalid result"
    parseRound _ = error "invalid round"

    parseHand h
      | h == 'A' = Rock
      | h == 'B' = Paper
      | h == 'C' = Scissors
      | otherwise = error "invalid hand"

scoreRound :: Round -> Int
scoreRound (a, b) = victoryPoints + handPoints
  where
    victoryPoints = case winningHand (a, b) of
      Just h | h == a -> 0
      Nothing -> 3
      Just h | h == b -> 6
      _ -> error "impossible"
    handPoints = case b of
      Rock -> 1
      Paper -> 2
      Scissors -> 3

winningHand :: Round -> Maybe Hand
winningHand (a, b)
  | b == strongerHand a = Just b
  | a == strongerHand b = Just a
  | otherwise = Nothing

strongerHand :: Hand -> Hand
strongerHand Rock = Paper
strongerHand Paper = Scissors
strongerHand Scissors = Rock

weakerHand :: Hand -> Hand
weakerHand h = fromJust $ find ((== h) . strongerHand) [Rock, Paper, Scissors]
