module Main (main) where

import Data.IntervalSet as S
import System.Exit

-- should fuse to id
test :: [Int] -> [Int]
test x = toList (S.map id (fromList x))

main :: IO ()
main = do
  print $ test []
  exitSuccess