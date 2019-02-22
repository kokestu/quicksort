module Quicksort where

import Data.Maybe
import Text.Read

main :: IO ()
main = do
  l <- getLine
  let xs = mapMaybe (readMaybe :: String -> Maybe Int) $ words l
  putStrLn $ show $ quicksort xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = (quicksort xs1) ++ [p] ++ (quicksort xs2)
  where
    (xs1, xs2) = partition p xs

    partition :: Ord a => a -> [a] -> ([a], [a])
    partition pivot =
      foldl
        (\(xs1, xs2) x ->
          if x > pivot
          then (xs1, x : xs2)
          else (x : xs1, xs2)
        )
        ([], [])