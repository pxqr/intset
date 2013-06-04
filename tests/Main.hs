{-# OPTIONS -fno-warn-orphans #-}
module Main (main) where

import Control.Applicative hiding (empty)
import Test.QuickCheck hiding ((.&.))
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.List as L (sort, nub, map, filter, minimum, intersect)
import Data.IntervalSet as S
import Data.IntervalSet.ByteString as S
import Data.Monoid
import Debug.Trace


instance Arbitrary IntSet where
  arbitrary = oneof [fromList <$> arbitrary, buddy <$> arbitrary]
    where
      buddy :: [Int] -> IntSet
      buddy = fromList . concatMap mk
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

prop_sorted :: IntSet -> Bool
prop_sorted xs = toList xs == L.nub (L.sort (toList xs))

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

prop_differenceMember :: IntSet -> IntSet -> Bool
prop_differenceMember a b = all (`notMember` difference a b) (toList b)

prop_differenceIntersection :: IntSet -> IntSet -> Bool
prop_differenceIntersection a b = (difference a b `intersection` b) == empty

prop_differenceSize :: IntSet -> IntSet -> Bool
prop_differenceSize a b = size (difference a b) <= size a


prop_differenceSubset :: IntSet -> IntSet -> Bool
prop_differenceSubset = undefined


prop_differenceDeMorgan1 :: IntSet -> IntSet -> IntSet -> Bool
prop_differenceDeMorgan1 a b c = a - b * c == (a - b) + (a - c)

prop_differenceDeMorgan2 :: IntSet -> IntSet -> IntSet -> Bool
prop_differenceDeMorgan2 a b c = a - (b + c) == (a - b) * (a - c)

prop_differenceDistributive :: IntSet -> IntSet -> IntSet -> Bool
prop_differenceDistributive a b c = (a + b) - c == (a - c) + (b - c)

prop_splitPivot :: IntSet -> Key -> Bool
prop_splitPivot s k = all (< k) (toList lt) && all (k <) (toList gt)
  where
    (lt, gt) = split k s

prop_splitIntersect :: IntSet -> Key -> Bool
prop_splitIntersect s k = lt * gt == empty
  where
    (lt, gt) = split k s

prop_splitGT :: IntSet -> Key -> Bool
prop_splitGT s k = all (k <) (toList (splitGT k s))

prop_splitLT :: IntSet -> Key -> Bool
prop_splitLT s k = all (< k) (toList (splitLT k s))

prop_interval :: Int -> Int -> Bool
prop_interval a s = interval l r == fromList [l..r]
  where
    l = a
    r = l + min 10000 s

prop_cmp :: IntSet -> IntSet -> Bool
prop_cmp a b = compare a b == compare (toList a) (toList b)

prop_numInst :: Int -> Bool
prop_numInst i = fromIntegral i == singleton i

prop_deleteEmpty :: Int -> Bool
prop_deleteEmpty k = delete k empty == empty

prop_elems :: IntSet -> Bool
prop_elems s = toList s == elems s

prop_combine :: [IntSet] -> Bool
prop_combine xs = all check xs
  where
    check x = us <> x == us && is * x == is
    us = mconcat xs
    is = intersections xs

prop_sortIdemp :: [Int] -> Bool
prop_sortIdemp xs = let a = ssort xs in ssort a == a
  where
    ssort = toList . fromList

prop_bitmapEncode :: IntSet -> Bool
prop_bitmapEncode xs = fromByteString (toByteString xs') == xs'
  where -- we should restrict upper bound otherwise we might have out of memory
    xs' = splitGT (-1) $ splitLT 1000000 xs

prop_partition :: IntSet -> Bool
prop_partition s = fst (S.partition even s) == S.filter even s
               &&  snd (S.partition even s) == S.filter (not . even) s

prop_subsetSize :: IntSet -> IntSet -> Bool
prop_subsetSize a b
  | a `isSubsetOf` b = size a <= size b
  |     otherwise    = True

prop_supersetSize :: IntSet -> IntSet -> Bool
prop_supersetSize a b
  | a `isSupersetOf` b = size a >= size b
  |    otherwise       = True

prop_subsetSuperset :: IntSet -> IntSet -> Bool
prop_subsetSuperset a b = (a `isSubsetOf` b) || (b `isSubsetOf` a)
                          || not (S.null (symDiff a b))

prop_subsetIntersection :: IntSet -> IntSet -> Bool
prop_subsetIntersection a b = (i `isSubsetOf` a) && (i `isSubsetOf` b)
  where
    i = a * b

prop_subsetUnion :: IntSet -> IntSet -> Bool
prop_subsetUnion a b = (a `isSubsetOf` u) && (b `isSubsetOf` u)
  where
    u = a + b


prop_symDiffLeftNeutral :: IntSet -> Bool
prop_symDiffLeftNeutral a = symDiff empty a == a

prop_symDiffRightNeutral :: IntSet -> Bool
prop_symDiffRightNeutral a = symDiff a empty == a

prop_symDiffCommutative :: IntSet -> IntSet -> Bool
prop_symDiffCommutative a b = (a `symDiff` b) == b `symDiff` a

prop_symDiffAssociative :: IntSet -> IntSet -> IntSet -> Bool
prop_symDiffAssociative a b c = ((a `symDiff` b) `symDiff` c)
                             == (a `symDiff` (b `symDiff` c))

prop_symDiffDistr :: IntSet -> IntSet -> IntSet -> Bool
prop_symDiffDistr a b c = (a `intersection` (b  `symDiff` c))
                       == ((a `intersection` b) `symDiff` (a `intersection` c))

prop_symDiffUnion :: IntSet -> IntSet -> Bool
prop_symDiffUnion a b = (a `difference` b) `union` (b `difference` a)
                     == symDiff a b

prop_symDiffInter :: IntSet -> IntSet -> Bool
prop_symDiffInter a b = (a `union` b) `difference` (a `intersection` b)
  == symDiff a b

main :: IO ()
main = defaultMain
  [ testProperty "empty"                prop_empty
  , testProperty "singleton"            prop_singleton
  , testProperty "insertLookup"         prop_insertLookup
  , testProperty "insert delete"        prop_insertDelete
  , testProperty "compare"              prop_cmp
  , testProperty "interval"             prop_interval

  , testProperty "subset size"          prop_subsetSize
  , testProperty "superset size"        prop_supersetSize
  , testProperty "subset superset exclusion"  prop_subsetSuperset
  , testProperty "subset of intersection"     prop_subsetIntersection
  , testProperty "subset of union"            prop_subsetUnion

  , testProperty "bitmap_encode"        prop_bitmapEncode

  , testProperty "symmetric difference left neutral"  prop_symDiffLeftNeutral
  , testProperty "symmetric difference right neutral" prop_symDiffRightNeutral
  , testProperty "symmetric difference commutative"   prop_symDiffCommutative
  , testProperty "symmetric difference associative"   prop_symDiffAssociative
  , testProperty "symmetric difference distributive"  prop_symDiffDistr
  , testProperty "symmetric difference union"         prop_symDiffUnion
  , testProperty "symmetric difference intersection"  prop_symDiffInter


--  , testProperty "universe member"      prop_universeMember
--  , testProperty "universe delete"      prop_universeDelete
--  , testProperty "universe insert"      prop_universeInsert
--  , testProperty "universe nat neg"     prop_universeNatNeg

  , testProperty "naturals"             prop_naturals
  , testProperty "negatives"            prop_negatives

  , testProperty "size"                 prop_size
  , testProperty "sort"                 prop_sorted
  , testProperty "sort idempotent"      prop_sortIdemp


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

  , testProperty "split pivot"               prop_splitPivot
  , testProperty "split intersection"        prop_splitIntersect
  , testProperty "split greater than"        prop_splitGT
  , testProperty "split lesser  than"        prop_splitLT
  , testProperty "partition filter"          prop_partition

  , testProperty "min"                  prop_min
  , testProperty "valid"                prop_valid

    -- for coverage mostly
  , testProperty "delete from empty"    prop_deleteEmpty
  , testProperty "combine"              prop_combine
  , testProperty "num instance"         prop_numInst
  , testProperty "elems"                prop_elems
  ]
