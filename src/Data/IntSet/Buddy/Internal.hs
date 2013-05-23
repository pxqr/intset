-- TODO document space complexity
-- TODO use key type

-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   See documentation for module header in Data.IntSet.Buddy.
--
{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable #-}
#endif

-- TODO use 'seq' instead of bang patterns
{-# LANGUAGE BangPatterns #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif

module Data.IntSet.Buddy.Internal
       (
         -- * Types
         IntSet, Key

         -- * Query
       , Data.IntSet.Buddy.Internal.null
       , size
       , member, notMember

         -- * Construction
       , empty
       , singleton
       , insert

         -- * Map/Fold/Filter
       , Data.IntSet.Buddy.Internal.map
       , Data.IntSet.Buddy.Internal.foldr
       , Data.IntSet.Buddy.Internal.filter

         -- * Combine
       , union, unions

         -- * Conversion
         -- ** Lists
       , elems
       , toList, fromList

         -- do not export this in Data.IntSet.Buddy
         -- * Internal
       , tip

         -- ** Debug
       , binCount, tipCount, finCount
       , wordCount
       ) where

import Data.Bits
import Data.Int
import Data.Word
import qualified Data.List as L
import Data.Typeable
import Data.Data


type Prefix = Int
type Mask   = Int
type BitMap = Word

-- | Type of IntSet elements.
type Key    = Int

-- TODO document each constructor
-- | Integer set.
data IntSet = Bin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask   !IntSet !IntSet
            | Tip {-# UNPACK #-} !Prefix {-# UNPACK #-} !BitMap
            | Fin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask
            | Nil
              deriving (Show
#if __GLASGOW_HASKELL__
                       , Typeable, Data
#endif
                       )
-- TODO document invariants


-- | /O(1)/. The empty set.
empty :: IntSet
empty = Nil
{-# INLINE empty #-}

-- | /O(1)/. A set containing one element.
singleton :: Key -> IntSet
singleton x = Tip (prefixOf x) (bitmapOf x)
{-# INLINE singleton #-}

-- | /O(1)/. Is this the empty set?
null :: IntSet -> Bool
null = undefined

-- | Cardinality of a set.
--
--   Worst case /O(n)/
--   Best case /O(1)/
--
size :: IntSet -> Int
size (Bin _  _  l r) = size l + size r
size (Tip _  bm    ) = popCount bm
size (Fin _  m     ) = m
size  Nil            = 0

-- | Test if the value is element of the set.
--
--   Worst case: /O(min(W, n))/
--   Best case:  /O(1)/
--
member :: Key -> IntSet -> Bool
member !x = go
  where
    go (Bin p m l r)
      | nomatch x p m = False
      |    zero x m   = go l
      |   otherwise   = go r

      -- FIX why not testBit bm (bitmapOf x)  ?
    go (Tip y  bm)   = prefixOf x == y && bitmapOf x .&. bm /= 0
    go (Fin p  m)    = match x p m && m .&. x == m
    go  Nil          = False

-- | Test if the value is not an element of the set.
--
--   Worst case: /O(min(W, n))/
--   Best case:  /O(1)/
--
notMember :: Key -> IntSet -> Bool
notMember !x = not . member x
{-# INLINE notMember #-}

-- | Add a value to the set.
insert :: Key -> IntSet -> IntSet
insert !x = insertBM (prefixOf x) (bitmapOf x)

insertBM :: Prefix -> BitMap -> IntSet -> IntSet
insertBM !kx !bm = go
  where -- do not use worker; use wrapper in go
    go t@(Bin p m l r)
      | nomatch kx p m = join kx (Tip kx bm) p t
      |    zero kx m   = binI p m (insertBM kx bm l) r
      |    otherwise   = binI p m l (insertBM kx bm r)

    go t@(Tip kx' bm')
      | kx' == kx = Tip kx (bm .|. bm')
      | otherwise = join kx (Tip kx bm) kx' t

    go t@(Fin   p m  )
      | nomatch kx p (m `div` 2) = join kx (Tip kx bm) p t
      |    otherwise         = t

    go    Nil          = Tip kx bm

{--------------------------------------------------------------------
  Combine
--------------------------------------------------------------------}
union :: IntSet -> IntSet -> IntSet
union t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
    -- TODO make platform endian independent
    | m1  > m2  = leftiest
    | m1  < m2  = rightiest
    | p1 == p2  = binI p1 m1 (union l1 l2) (union r1 r2)
    | otherwise = join p1 t1 p2 t2
  where
    leftiest
      | nomatch p2 p1 m1 = join p1 t1 p2 t2
      |     zero p2 m1   = binI p1 m1 (union l1 t2) r1
      |      otherwise   = binI p1 m1 l1 (union r1 t2)

    rightiest
      | nomatch p2 p1 m1 = join p1 t1 p2 t2
      |    zero p1 m2    = binI p2 m2 (union t1 l2) r2
      |     otherwise    = binI p2 m2 l2 (union t1 r2)

union t @(Bin _ _ _ _)      (Tip p bm) = insertBM p bm t
union t1@(Bin p1 m1 _ _) t2@(Fin p2 m2)
  | m1 < m2 && mask m2 p1 == mask m2 p2 = Fin p2 m2
  | otherwise = join p1 t1 p2 t2

union t@(Bin _ _ _ _)  Nil       = t

union (Tip p bm) t = insertBM p bm t
-- TODO implement this case
union (Fin p m)  t = undefined
union  Nil t       = t

unions :: [IntSet] -> IntSet
unions = L.foldl' union empty

{--------------------------------------------------------------------
  Min/max
--------------------------------------------------------------------}
findMin :: IntSet -> Key
findMin (Bin _ rootM l r)
-- TODO is it correct?
    | rootM < 0 = go r
    | otherwise = go l
  where
    go (Bin _ m l r) = go l
    go (Tip p bm)    = undefined
    go (Fin p _)     = p
    go  Nil          = error "findMax.go: Bin Nil invariant failed"

findMin (Tip p bm) = p + findMinBM bm
findMin (Fin p _)  = p
findMin  Nil       = error "findMin: empty set"

-- TODO implement findMinBM
findMinBM :: BitMap -> Int
findMinBM _ = error "findMinBM"
{-# INLINE findMinBM #-}


findMax :: IntSet -> Key
findMax (Bin _ rootM l r)
    | rootM < 0 = go l
    | otherwise = go r
  where
    go (Bin _ _ child _) = go child
    go (Tip p bm)        = p + findMaxBM bm
    go (Fin p m)         = p + m - 1
    go  Nil              = error "findMax.go: Bin Nil invariant failed"

findMax (Tip p bm) = p + findMaxBM bm
findMax (Fin p m ) = p + m - 1
findMax  Nil       = error "findMax: empty set"

-- TODO implement findMaxBM
findMaxBM :: BitMap -> Int
findMaxBM = error "findMaxBM"
{-# INLINE findMaxBM #-}

{--------------------------------------------------------------------
   Map/fold/filter
--------------------------------------------------------------------}
-- TODO fusion
map :: (Key -> Key) -> IntSet -> IntSet
map f = fromList . L.map f . toList
{-# INLINE map #-}

listFin :: Prefix -> Mask -> [Key]
listFin p m = [p..p + m - 1]

foldr :: (Key -> a -> a) -> a -> IntSet -> a
foldr f a = wrap
  where
    wrap (Bin _ m l r)
      |   m > 0   = go (go a r) l
      | otherwise = go (go a l) r
    wrap t = go a t

    go z (Bin _ _ l r) = go (go z r) l
    go z (Tip p bm)    = foldrBits p f z bm
    go z (Fin p m)     = L.foldr f z (listFin p m)
    go z  Nil          = z

filter :: (Key -> Bool) -> IntSet -> IntSet
filter f = go
  where
    -- we use Bin instead of bin because
    go (Bin p m l r) = Bin m p (go l) (go r)
    -- FIX fromDistinctAscList
    go (Tip p bm) = undefined
    go (Fin p m)  = fromList $ L.filter f $ listFin p m
                    --fromList (L.filter f (listBuddies m p))
    go  Nil       = Nil

{--------------------------------------------------------------------
  List conversions
--------------------------------------------------------------------}
fromList :: [Key] -> IntSet
fromList = L.foldl' (flip insert) empty

toList :: IntSet -> [Key]
toList = Data.IntSet.Buddy.Internal.foldr (:) []

elems :: IntSet -> [Key]
elems = toList
{-# INLINE elems #-}

{--------------------------------------------------------------------
  Smart constructors
--------------------------------------------------------------------}
tip :: Prefix -> BitMap -> IntSet
tip _ 0  = Nil
tip p bm = Tip p bm
{-# INLINE tip #-}

-- TODO make binIL, binIR for left and right subtrees exclusively;
-- and see if this gives some boost
--
-- used when we insert in left or right subtree tree
binI :: Prefix -> Mask -> IntSet -> IntSet -> IntSet
binI _ _ (Tip p1 bm1) (Tip p2 bm2)
  | isFull bm1 && isFull bm2 && xor p1 p2 == 64
  = Fin p1 128

binI p m (Fin _  m1) (Fin _  m2)
  | m1 == m && m2 == m
  = Fin p (m * 2)

binI p m l r = Bin p m l r


join :: Prefix -> IntSet -> Prefix -> IntSet -> IntSet
join p1 t1 p2 t2
    | zero p1 m = Bin p m t1 t2
    | otherwise = Bin p m t2 t1
  where
    p = mask p1 m
    m = branchMask p1 p2
{-# INLINE join #-}


{--------------------------------------------------------------------
  Debug
--------------------------------------------------------------------}
binCount :: IntSet -> Int
binCount (Bin _ _ l r) = binCount l + binCount r
binCount _             = 0

tipCount :: IntSet -> Int
tipCount (Bin _ _ l r) = tipCount l + tipCount r
tipCount (Tip _ _)     = 1
tipCount _             = 0

finCount :: IntSet -> Int
finCount (Bin _ _ l r) = finCount l + finCount r
finCount (Fin _ _)     = 1
finCount _             = 0

wordCount :: IntSet -> Int
wordCount (Bin _ _ l r) = 5 + wordCount l + wordCount r
wordCount (Tip _ _)     = 3
wordCount (Fin _ _)     = 3
wordCount  Nil          = 1

{--------------------------------------------------------------------
  Misc
--------------------------------------------------------------------}
-- TODO specialize for internal use cases
foldrBits :: Int -> (Int -> a -> a) -> a -> BitMap -> a
foldrBits p f acc bm = go 0
  where
    go i
      |   i == 64    = acc
      | testBit bm i = f (p + i) (go (succ i))
      |   otherwise  = go (succ i)
{-# SPECIALIZE
  foldrBits :: Int
            -> (Int -> IntSet -> IntSet)
            -> IntSet -> BitMap -> IntSet
  #-}

isFull :: BitMap -> Bool
isFull x = complement 0 .&. x == complement 0
{-# INLINE isFull #-}



{--------------------------------------------------------------------
  All later code is taken from Data.IntSet.Base
--------------------------------------------------------------------}
type Nat = Word

natFromInt :: Int -> Nat
natFromInt = fromIntegral
{-# INLINE natFromInt #-}

intFromNat :: Nat -> Int
intFromNat = fromIntegral
{-# INLINE intFromNat #-}

{--------------------------------------------------------------------
  Endian independent bit twiddling
--------------------------------------------------------------------}
zero :: Int -> Mask -> Bool
zero i m = (natFromInt i .&. natFromInt m) == 0
{-# INLINE zero #-}

-- Suppose a is largest such that 2^a divides 2*m.
-- Then mask i m is i with the low a bits zeroed out.
mask :: Int -> Mask -> Prefix
mask i m
  = maskW (natFromInt i) (natFromInt m)
{-# INLINE mask #-}

match :: Int -> Prefix -> Mask -> Bool
match i p m = mask i m == p
{-# INLINE match #-}

nomatch :: Int -> Prefix -> Mask -> Bool
nomatch i p m = mask i m /= p
{-# INLINE nomatch #-}

shorter :: Mask -> Mask -> Bool
shorter m1 m2 = natFromInt m1 > natFromInt m2
{-# INLINE shorter #-}

branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
  = intFromNat (highestBitMask (natFromInt p1 `xor` natFromInt p2))
{-# INLINE branchMask #-}

-- make platform independent
-- | Return a word where only the highest bit is set.
highestBitMask :: Word -> Word
highestBitMask x1 =
  let x2 = x1 .|. x1 `shiftR` 1
      x3 = x2 .|. x2 `shiftR` 2
      x4 = x3 .|. x3 `shiftR` 4
      x5 = x4 .|. x4 `shiftR` 8
      x6 = x5 .|. x5 `shiftR` 16
-- #if !(defined(__GLASGOW_HASKELL__) && WORD_SIZE_IN_BITS==32)
      x7 = x6 .|. x6 `shiftR` 32
  in x7 `xor` (x7 `shiftR` 1)
-- #else
--in x6 `xor` (x6 `shiftRL` 1)
-- #endif
{-# INLINE highestBitMask #-}

{--------------------------------------------------------------------
  Big endian operations
--------------------------------------------------------------------}
maskW :: Nat -> Nat -> Prefix
maskW i m = intFromNat (i .&. (complement (m-1) `xor` m))
{-# INLINE maskW #-}


{----------------------------------------------------------------------
  Functions that generate Prefix and BitMap of a Key or a Suffix.
----------------------------------------------------------------------}

suffixBitMask :: Int
suffixBitMask = bitSize (undefined :: Word) - 1
{-# INLINE suffixBitMask #-}

prefixBitMask :: Int
prefixBitMask = complement suffixBitMask
{-# INLINE prefixBitMask #-}

prefixOf :: Int -> Prefix
prefixOf x = x .&. prefixBitMask
{-# INLINE prefixOf #-}

suffixOf :: Int -> Int
suffixOf x = x .&. suffixBitMask
{-# INLINE suffixOf #-}

bitmapOfSuffix :: Int -> BitMap
bitmapOfSuffix s = 1 `shiftL` s
{-# INLINE bitmapOfSuffix #-}

bitmapOf :: Int -> BitMap
bitmapOf x = bitmapOfSuffix (suffixOf x)
{-# INLINE bitmapOf #-}
