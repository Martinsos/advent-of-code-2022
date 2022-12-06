#!/usr/bin/env cabal
{- cabal:
  build-depends: base >= 4.7 && < 5
-}

import Data.List (nub)

main :: IO ()
main = do
  readFile "input.txt" >>= print . findMarker 4 -- 1876
  readFile "input.txt" >>= print . findMarker 14 -- 2202

findMarker :: Int -> String -> Int
findMarker n ds = go (take (n - 1) ds) (zip [n ..] $ drop (n - 1) ds)
  where
    go lastN ((i, curr) : next) =
      let lastN' = take n (curr : lastN)
       in if nub lastN' == lastN' then i else go lastN' next
    go _ _ = error "no marker found"
