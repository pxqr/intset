{-# OPTIONS -fno-warn-orphans #-}
module Main (main) where

import Control.Applicative hiding (empty)
import Test.QuickCheck hiding ((.&.))
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.List as L (sort, nub, map)
import Data.IntSet.Buddy
import Data.Monoid


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

prop_sortBS :: [Int] -> Bool
prop_sortBS = undefined -- TODO

prop_valid :: IntSet -> Bool
prop_valid = isValid

prop_unionComm :: IntSet -> IntSet -> Bool
prop_unionComm a b = a <> b == b <> a

prop_unionAssoc :: IntSet -> IntSet -> IntSet -> Bool
prop_unionAssoc a b c = a <> (b <> c) == (a <> b) <> c

prop_unionLeftId :: IntSet -> Bool
prop_unionLeftId a = mempty <> a == a

prop_unionRightId :: IntSet -> Bool
prop_unionRightId a = mempty <> a == a

prop_showRead :: IntSet -> Bool
prop_showRead a = read (show a) == a

prop_eq :: [Int] -> [Int] -> Bool
prop_eq a b
    | a' == b' = fromList a' == fromList b'
    | a' /= b' = fromList a' /= fromList b'
  where
    a' = nub (sort a)
    b' = nub (sort b)

prop_size :: [Int] -> Bool
prop_size xs = length (nub (sort xs)) == size (fromList xs)

prop_insertDelete :: Int -> IntSet -> Bool
prop_insertDelete i = notMember i . delete i . insert i

main :: IO ()
main = defaultMain
  [ testProperty "empty"                prop_empty
  , testProperty "singleton"            prop_singleton
  , testProperty "insertLookup"         prop_insertLookup
  , testProperty "insert delete"        prop_insertDelete
  , testProperty "unionLookup"          prop_unionLookup
  , testProperty "size"                 prop_size
  , testProperty "sort"                 prop_sort
  , testProperty "valid"                prop_valid

  , testProperty "read . show == id"    prop_showRead
  , testProperty "equality"             prop_eq

  , testProperty "union commutative"    prop_unionComm
  , testProperty "union associative"    prop_unionAssoc
  , testProperty "union left identity"  prop_unionLeftId
  , testProperty "union right identity" prop_unionRightId
  ]
