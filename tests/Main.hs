{-# OPTIONS -fno-warn-orphans #-}
module Main (main) where

import Control.Applicative hiding (empty)
import Test.QuickCheck hiding ((.&.))
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.List as L (sort, nub, map)
import Data.IntSet.Buddy


instance Arbitrary IntSet where
  arbitrary = fromList <$> arbitrary

prop_empty :: [Int] -> Bool
prop_empty xs = (not . (`member` empty)) `all` xs

prop_singleton :: Int -> [Int] -> Bool
prop_singleton e = all check
  where
    check x |   x == e  = member    x (singleton e)
            | otherwise = notMember x (singleton e)

prop_insertLookup :: IntSet -> Int -> Bool
prop_insertLookup s i = member i (insert i s )

prop_unionLookup :: [Int] -> [Int] -> Bool
prop_unionLookup a b = and
   [ all (`member` u) a
   , all (`member` u) b
   ]
  where
    u = union (fromList a) (fromList b)

prop_sort :: [Int] -> Bool
prop_sort xs' = toList (fromList xs) == L.nub (L.sort xs)
  where
    xs = L.map abs xs'

main :: IO ()
main = defaultMain
  [ testProperty "empty"        prop_empty
  , testProperty "singleton"    prop_singleton
  , testProperty "insertLookup" prop_insertLookup
  , testProperty "unionLookup"  prop_unionLookup
  , testProperty "sort"         prop_sort
  ]
