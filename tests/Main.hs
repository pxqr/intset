{-# OPTIONS -fno-warn-orphans #-}
module Main (main) where

import Control.Applicative hiding (empty)
import Test.QuickCheck hiding ((.&.))
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.List as L (sort, nub, map, filter, minimum, intersect)
import Data.IntSet.Buddy as S
import Data.Monoid


instance Arbitrary IntSet where
--  arbitrary = fromList <$> arbitrary
  arbitrary = buddy <$> arbitrary
    where
      buddy :: [Int] -> IntSet
      buddy = fromList . concatMap (mk . abs)
        where
          mk i = [i * 64 .. i * 64 + 64]

  shrink (Bin _ _ l r) = [l, r]
  shrink (Fin p m)     = [splitFin p m]
  shrink  _            = []

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

prop_valid :: IntSet -> Bool
prop_valid = isValid

prop_unionSize :: IntSet -> IntSet -> Bool
prop_unionSize a b = size u >= size a
                  && size u >= size b
                  && size u <= size a + size b
  where
    u = union a b

prop_unionComm :: IntSet -> IntSet -> Bool
prop_unionComm a b = a <> b == b <> a

prop_unionAssoc :: IntSet -> IntSet -> IntSet -> Bool
prop_unionAssoc a b c = a <> (b <> c) == (a <> b) <> c

prop_unionLeftId :: IntSet -> Bool
prop_unionLeftId a = mempty <> a == a

prop_unionRightId :: IntSet -> Bool
prop_unionRightId a = mempty <> a == a

prop_unionTop :: IntSet -> Bool
prop_unionTop a = a <> a == a

prop_unionIdemp :: IntSet -> IntSet -> Bool
prop_unionIdemp a b = ((a <> b) <> b) == a <> b

prop_intersectionSize :: IntSet -> IntSet -> Bool
prop_intersectionSize a b = size i <= size a && size i <= size b
  where
    i = intersection a b

prop_intersection :: [Int] -> [Int] -> Bool
prop_intersection a b =
  intersection (fromList a) (fromList b)
  == fromList (L.intersect a b)

prop_intersectComm :: IntSet -> IntSet -> Bool
prop_intersectComm a b = (a `intersection` b) == (b `intersection` a)

prop_intersectAssoc :: IntSet -> IntSet -> IntSet -> Bool
prop_intersectAssoc a b c =
  ((a `intersection` b) `intersection` c)
  ==  (a `intersection` (b `intersection` c))

prop_intersectLeft :: IntSet -> Bool
prop_intersectLeft a = intersection empty a == empty

prop_intersectRight :: IntSet -> Bool
prop_intersectRight a = intersection a empty == empty

prop_intersectBot :: IntSet -> Bool
prop_intersectBot a = (a `intersection` a) == a

prop_intersectIdemp :: IntSet -> IntSet -> Bool
prop_intersectIdemp a b = ((a `intersection` b) `intersection` b)
                          == intersection a b

prop_showRead :: IntSet -> Bool
prop_showRead a = read (show a) == a

prop_eq :: [Int] -> [Int] -> Bool
prop_eq a b
    | a' == b' = fromList a' == fromList b'
    | a' /= b' = fromList a' /= fromList b'
    | otherwise = error "prop_eq: impossible"
  where
    a' = nub (sort a)
    b' = nub (sort b)

prop_eqRefl :: IntSet -> Bool
prop_eqRefl a = a == a

prop_eqSym :: IntSet -> IntSet -> Bool
prop_eqSym a b = (a == b) == (b == a)

prop_eqTrans :: IntSet -> IntSet -> IntSet -> Bool
prop_eqTrans a b c
  | (a == b) && (b == c) = a == c
  |    otherwise         = True

prop_size :: [Int] -> Bool
prop_size xs = length (nub (sort xs)) == size (fromList xs)

prop_insertDelete :: Int -> IntSet -> Bool
prop_insertDelete i = notMember i . delete i . insert i

prop_mapPresSize :: IntSet -> Bool
prop_mapPresSize s = size (S.map (*2) s) == size s

prop_mapLessSize :: IntSet -> Bool
prop_mapLessSize s = size (S.map (`div` 2) s) <= size s

prop_mapping :: [Int] -> Bool
prop_mapping xs = toList (S.map (*2) (fromList xs)) == L.map (*2) (nub (sort xs))

prop_filterSize :: IntSet -> Bool
prop_filterSize s = size (S.filter even s) <= size s

prop_filtering :: [Int] -> Bool
prop_filtering xs = S.filter even (fromList xs) == fromList (L.filter even xs)

prop_min :: [Int] -> Bool
prop_min [] = True
prop_min xs = findMin (fromList xs) == L.minimum xs

prop_universeMember :: [Int] -> Bool
prop_universeMember = all (`member` universe)

prop_universeDelete :: Int -> Bool
prop_universeDelete i = notMember i (delete i universe)

prop_universeInsert :: Int -> Bool
prop_universeInsert i = insert i universe == universe

prop_universeNatNeg :: Bool
prop_universeNatNeg = naturals <> negatives == universe

prop_naturals :: [Int] -> Bool
prop_naturals = all check
  where
    check x |   x >= 0  = member x naturals
            | otherwise = True

prop_negatives :: [Int] -> Bool
prop_negatives = all check
  where
    check x |   x < 0   = member x negatives
            | otherwise = True

prop_minInSet :: IntSet -> Bool
prop_minInSet s
  |  S.null s = True
  | otherwise = member (findMin s) s

-- TODO tests specialized for Fin
-- TODO tests for universy

prop_differenceMember :: IntSet -> IntSet -> Bool
prop_differenceMember a b = all (`notMember` difference a b) (toList b)

prop_differenceIntersection :: IntSet -> IntSet -> Bool
prop_differenceIntersection a b = (difference a b `intersection` b) == empty

prop_differenceSize :: IntSet -> IntSet -> Bool
prop_differenceSize a b = size (difference a b) <= size a

prop_differenceSubset :: IntSet -> IntSet -> Bool
prop_differenceSubset = undefined
-- TODO implement subset

prop_differenceDeMorgan1 :: IntSet -> IntSet -> IntSet -> Bool
prop_differenceDeMorgan1 a b c = a - b * c == (a - b) + (a - c)

prop_differenceDeMorgan2 :: IntSet -> IntSet -> IntSet -> Bool
prop_differenceDeMorgan2 a b c = a - (b + c) == (a - b) * (a - c)

prop_differenceDistributive :: IntSet -> IntSet -> IntSet -> Bool
prop_differenceDistributive a b c =
     ((a `union` b) `difference` c)
  == ((a `difference` c) `union` (b `difference` c))


main :: IO ()
main = defaultMain
  [ testProperty "empty"                prop_empty
  , testProperty "singleton"            prop_singleton
  , testProperty "insertLookup"         prop_insertLookup
  , testProperty "insert delete"        prop_insertDelete

--  , testProperty "universe member"      prop_universeMember
--  , testProperty "universe delete"      prop_universeDelete
--  , testProperty "universe insert"      prop_universeInsert
--  , testProperty "universe nat neg"     prop_universeNatNeg

  , testProperty "naturals"             prop_naturals
  , testProperty "negatives"            prop_negatives

  , testProperty "size"                 prop_size
  , testProperty "sort"                 prop_sort


  , testProperty "read . show == id"    prop_showRead

  , testProperty "equality"              prop_eq
  , testProperty "equality reflexivity"  prop_eqRefl
  , testProperty "equality symmetry"     prop_eqSym
  , testProperty "equality transitivity" prop_eqTrans

  , testProperty "map preserve size"    prop_mapPresSize
  , testProperty "map not preserve siz" prop_mapLessSize
  , testProperty "mapping"              prop_mapping

  , testProperty "filter size"          prop_filterSize
  , testProperty "filtering"            prop_filtering

  , testProperty "union size"           prop_unionSize
  , testProperty "unionLookup"          prop_unionLookup
  , testProperty "union commutative"    prop_unionComm
  , testProperty "union associative"    prop_unionAssoc
  , testProperty "union left identity"  prop_unionLeftId
  , testProperty "union right identity" prop_unionRightId
  , testProperty "union top"            prop_unionTop
  , testProperty "union idemp"          prop_unionIdemp

  , testProperty "minima in set"        prop_minInSet

  , testProperty "intersection size"         prop_intersectionSize
  , testProperty "intersection lists"        prop_intersection
  , testProperty "intersection commutative"  prop_intersectComm
  , testProperty "intersection associative"  prop_intersectAssoc
  , testProperty "intersection idemp"        prop_intersectIdemp
  , testProperty "intersection left empty"   prop_intersectLeft
  , testProperty "intersection right empty"  prop_intersectRight
  , testProperty "intersection bot"          prop_intersectBot

  , testProperty "difference member"         prop_differenceMember
  , testProperty "difference intersection"   prop_differenceIntersection
  , testProperty "difference size"           prop_differenceSize
  , testProperty "difference de morgan1"     prop_differenceDeMorgan1
  , testProperty "difference de morgan2"     prop_differenceDeMorgan2
  , testProperty "difference distributive"   prop_differenceDistributive

  , testProperty "min"                  prop_min
  , testProperty "valid"                prop_valid

  ]
