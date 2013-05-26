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
-- TODO fix this
-- {-# LANGUAGE Safe #-}
#endif

module Data.IntSet.Buddy.Internal
       (
         -- * Types
         IntSet(..), Key

         -- * Query
       , Data.IntSet.Buddy.Internal.null
       , size
       , member, notMember

         -- * Construction
       , empty
       , singleton

       , naturals
       , negatives
       , universe

       , insert
       , delete

         -- * Map/Fold/Filter
       , Data.IntSet.Buddy.Internal.map
       , Data.IntSet.Buddy.Internal.foldr
       , Data.IntSet.Buddy.Internal.filter

         -- * Min/Max
       , findMin, findMax

         -- * Combine
       , union, unions
       , intersection, intersections
       , difference

         -- * Conversion
         -- ** Lists
       , elems
       , toList, fromList

         -- do not export this in Data.IntSet.Buddy
         -- * Internal
         -- ** Types
       , Prefix, Mask, BitMap
       , finMask, nomatch, match, mask

         -- ** Smart constructors
       , tip, tipI, tipD, bin
       , insertBM
       , unionBM

         -- ** Debug
       , shorter, prefixOf, bitmapOf, branchMask, matchFin

         -- *** Stats
       , binCount, tipCount, finCount
       , wordCount
       , savedSpace
       , ppStats

         -- *** Invariants
       , isValid

         -- *** Visualization
       , showTree, showRaw
       , putTree, putRaw
       ) where


import Control.DeepSeq
import Data.Bits as Bits
import Data.Bits.Extras
import Data.Data
import qualified Data.List as L
import Data.Monoid
import Data.Ord
import Data.Word
import Text.ParserCombinators.ReadP


-- | Prefix is used to distinguish subtrees by its prefix bits.  When
--   new prefix is created its non prefix bits are zeroed.  Prefix is
--   big endian. This means that we throw away only least significant
--   bits
type Prefix = Int

-- | Mask is used to specify mask for branching bit.
--   For exsample if we have mask 0000100 that means:
--
--     * We do not consider last three bits in prefix.
--
--     * Branching bit is at position 3 starting from least
--     significant bit.
--
--     * Prefix mask is 4 bits. (at left of the bitstring)
--
type Mask   = Int
-- TODO Mask = Word?

-- | Bitmap is used to make intset dense. To achive this we throw away
-- last bits 6 or 7 bits from any(!) prefix and thus any(!) mask
-- should be more than 64 or 32. Bitmap by itself contain flags which
-- indicates "is an element present in a set?" by marking suffices
-- indices. For exsample bitmap 01001010 contain elements 2, 5 and 7.
--
--   One bitmap might contain up to 64 or 32 (depending on arch)
-- elements.
--
type BitMap = Word

-- | Type of IntSet elements.
type Key    = Int


-- | Integer set.
data IntSet
  -- | Layout: prefix up to branching bit, mask for branching bit,
  --   left subtree and right subtree.
  --
  --   IntSet = Bin: contains elements of left and right subtrees thus
  --   just merge to subtrees.  All elements of left subtree is less
  --   that from right subtree. Except non-negative numbers, they are
  --   in left subtree of root bin, if any.
  --
  = Bin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask   !IntSet !IntSet

  -- | Layout: Prefix up to /mask of bitmap size/, and bitmap
  --   containing elements starting /from the prefix/.
  --
  --   IntSet = Tip: contains elements
  --
  | Tip {-# UNPACK #-} !Prefix {-# UNPACK #-} !BitMap

  -- | Layout: Prefix up to /mask of bitmap size/, and mask specifing
  --   how large is set. There is no branching bit at all.
  --   Tip is never full.
  --
  --   IntSet = Fin: contains all elements from prefix to
  --   (prefix - mask - 1)
  --
  | Fin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask

  -- | Empty set. Contains nothing.
  | Nil
  deriving
    ( Eq
#if __GLASGOW_HASKELL__
    , Typeable, Data
#endif
    )

{--------------------------------------------------------------------
  Invariants
--------------------------------------------------------------------}
-- |
--   + Nil is never child of Bin;
--   + Bin is never contain two Fins with masks equal to mask of Bin;
--   - Mask becomes smaller at each child;
--   - Bin, Fin, Tip masks is always power of two;
--   + Fin mask is always greater or equal than size of bits in word;
--   - Bin left subtree contains element each of which is less than each element
--     of right subtree;
--   + Tip bitmap is never full;
--   + Tip mask is multiple of word bit count;
--
--  See 'binI' to find out when two intsets should be merged into one.
--
-- TODO check 2,3,4 invariants
--
isValid :: IntSet -> Bool
isValid  Nil       = True
isValid (Tip p bm) = not (isFull bm) && (p `mod` 64 == 0)
isValid (Fin _ m ) = m >= 64
isValid (Bin _  _ Nil _  ) = error "Bin _ _ Nil _"
isValid (Bin _  _ _   Nil) = error "Bin _ _ _   Nil"
isValid (Bin _  m (Fin _ m1) (Fin _ m2))
  = not (m == m1 && m == m2)
isValid (Bin _  _ l r)
  = isValid l && isValid r

--isPowerOf2 x = (x - 1 .&. x) == 0

{--------------------------------------------------------------------
  Instances
--------------------------------------------------------------------}

instance Show IntSet where
  showsPrec _ s = showString "{" . list (toList s) . showString "}"
    where
      list [] = showString ""
      list [x] = shows x
      list (x : xs) = shows x . showString "," . list xs

instance Read IntSet where
  readsPrec _ = readP_to_S $ do
    "{" <- readS_to_P lex
    xs  <- readS_to_P reads `sepBy` (skipSpaces >> char ',')
    "}" <- readS_to_P lex
    return (fromList xs)

instance Ord IntSet where
  compare = comparing toList
  -- TODO make it faster

instance Monoid IntSet where
  mempty  = empty
  mappend = union
  mconcat = unions

instance Num IntSet where
  (+) = union
  (*) = intersection
  (-) = difference
  negate = Data.IntSet.Buddy.Internal.complement
  abs = error "IntSet.abs: not implemented"
  signum = error "IntSet.singum: not implemented"
  fromInteger = singleton . fromIntegral

instance Bounded IntSet where
  minBound = empty
  maxBound = universe

instance NFData IntSet where


{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(1)/. Is this the empty set?
null :: IntSet -> Bool
null Nil = True
null _   = False

-- | /O(n)/ or /O(1)/. Cardinality of a set.
size :: IntSet -> Int
size (Bin _  _  l r) = size l + size r
size (Tip _  bm    ) = popCount bm
size (Fin _  m     ) |    m > 0  = m
                     | otherwise = error "IntSet.size: int overflow"
size  Nil            = 0

-- | /O(min(W, n))/ or /O(1)/.
--   Test if the value is element of the set.
--
member :: Key -> IntSet -> Bool
member !x = go
  where
    go (Bin p m l r)
      | nomatch x p m = False
      |    zero x m   = go l
      |   otherwise   = go r

    go (Tip y  bm)   = prefixOf x == y && bitmapOf x .&. bm /= 0
    go (Fin p  m)    = p <= x && (x <= (p + m - 1))
    go  Nil          = False

-- | /O(min(W, n))/ or /O(1)/.
--   Test if the value is not an element of the set.
--
notMember :: Key -> IntSet -> Bool
notMember !x = not . member x
{-# INLINE notMember #-}

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}

-- | /O(1)/. The empty set.
empty :: IntSet
empty = Nil
{-# INLINE empty #-}

-- | /O(1)/. A set containing one element.
singleton :: Key -> IntSet
singleton x = Tip (prefixOf x) (bitmapOf x)
{-# INLINE singleton #-}

-- | /O(1)/. The set containing all natural numbers.
naturals :: IntSet
naturals = Fin 0 (bit 63)
{-# INLINE naturals #-}

-- | /O(1)/. The set containing all negative numbers.
negatives :: IntSet
negatives = Fin (bit 63) (bit 63)
{-# INLINE negatives #-}

-- | /O(1)/. The set containing the all integers it might contain.
universe :: IntSet
universe = Fin (bit 63) 0
{-# INLINE universe #-}

-- | /O(min(W, n)/ or /O(1)/. Add a value to the set.
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
      | kx' == kx = tipI kx (bm .|. bm')
      | otherwise = join kx (Tip kx bm) kx' t

    go t@(Fin   p m  )
      | nomatch kx p (finMask m) = join kx (Tip kx bm) p t
      |        otherwise          = t

    go    Nil          = Tip kx bm


-- | /O(min(n, W))/. Delete a value from the set.
delete :: Key -> IntSet -> IntSet
delete !x = deleteBM (prefixOf x) (bitmapOf x)

deleteBM :: Prefix -> BitMap -> IntSet -> IntSet
deleteBM !kx !bm = go
  where
    go t@(Bin p m l r)
      | nomatch kx p m = t
      |    zero kx m   = binD p m (deleteBM kx bm l) r
      |    otherwise   = binD p m l (deleteBM kx bm r)

    go t@(Tip kx' bm')
      | kx == kx'  = tipD kx (bm' .&. Bits.complement bm)
      | otherwise  = join kx (Tip kx bm) kx' t

    go t@(Fin p m) -- TODO delete 0 universe doesn't work
      | nomatch kx p (finMask m) = t
      |       otherwise          = deleteBM kx bm (splitFin p m)

    go    Nil      = Nil

splitFin :: Prefix -> Mask -> IntSet
splitFin p m
  -- WARN here we have inconsistency - bitmap is full
  -- but this will be fixed in next go for any kx bm
  -- and this faster (I think)
  |  m == 64  = Tip p (Bits.complement 0)
   | otherwise = Bin p m' (Fin p m') (Fin (p + m') m')
  where
    m' = intFromNat ((natFromInt m) `shiftR` 1) -- TODO endian independent


complement :: IntSet -> IntSet
complement Nil = universe
complement _   = error "complement: not implemented"


{--------------------------------------------------------------------
  Combine
--------------------------------------------------------------------}

{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}

infixl 6 `union`

-- | /O(n + m)/ or /O(1)/. The union of two sets.
union :: IntSet -> IntSet -> IntSet
union t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
    | shorter m1 m2 = leftiest
    | shorter m2 m1 = rightiest
    | p1 == p2      = binI p1 m1 (union l1 l2) (union r1 r2)
    | otherwise     = join p1 t1 p2 t2
  where
    leftiest
      | nomatch p2 p1 m1 = join p1 t1 p2 t2
      |     zero p2 m1   = binI p1 m1 (union l1 t2) r1
      |      otherwise   = binI p1 m1 l1 (union r1 t2)

    rightiest
      | nomatch p1 p2 m2 = join p1 t1 p2 t2
      |    zero p1 m2    = binI p2 m2 (union t1 l2) r2
      |     otherwise    = binI p2 m2 l2 (union t1 r2)

union t@(Bin _ _ _ _) (Tip p bm) = insertBM  p bm t
union t@(Bin _ _ _ _) (Fin p m ) = insertFin p m  t
union t@(Bin _ _ _ _)  Nil       = t
union   (Fin p m )     t         = insertFin p m t
union   (Tip p bm)     t         = insertBM p bm t
union    Nil           t         = t


insertFin :: Prefix -> Mask -> IntSet -> IntSet
insertFin p1 m1  t2@(Bin p2 m2 l r)
    | m2 `shorter` m1 = if zero p1 m2
                        then binI p2 m2 (insertFin p1 m1 l) r
                        else binI p2 m2 l (insertFin p1 m1 r)
    | match p2 p1 m1 -- m1 `shorterOrEqTo` m2
    = Fin p1 m1
    | otherwise = join p1 (Fin p1 m1) p2 t2

insertFin p1 m1 (Tip p bm) = insertBM p bm (Fin p1 m1)
insertFin p1 m1 (Fin p2 m2 )
    |    isBuddy p1 m1 p2 m2     = Fin p1 (m1 * 2)
    |    isBuddy p2 m2 p1 m1     = Fin p2 (m1 * 2)
    | properSubsetOf p1 m1 p2 m2 = Fin p2 m2
    | properSubsetOf p2 m2 p1 m1 = Fin p1 m1
    |         otherwise          = join p1 (Fin p1 m1) p2 (Fin p2 m2)

insertFin p m Nil = Fin p m

-- | The union of list of sets.
unions :: [IntSet] -> IntSet
unions = L.foldl' union empty


-- test if the two Fins is good to merge
isBuddy :: Prefix -> Mask -> Prefix -> Mask -> Bool
isBuddy p1 m1 p2 m2 = m1 == m2 && xor p1 p2 == m1 && p1 .&. m1 == 0
{-# INLINE isBuddy #-}

-- used to if one Fin is subset of the another Fin
properSubsetOf :: Prefix -> Mask -> Prefix -> Mask -> Bool
properSubsetOf p1 m1 p2 m2 = (m2 `shorter` m1) && matchFin p1 p2 m2
{-# INLINE properSubsetOf #-}

unionBM :: Prefix -> BitMap -> IntSet -> IntSet
unionBM p bm t = case tip p bm of
  Bin _ _ _ _ -> error "unionBM: impossible"
  Fin p' m'   -> insertFin p' m' t
  Tip p' bm'  -> insertBM p' bm' t
  Nil         -> t

{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}

infixl 7 `intersection`

-- | /O(n + m)/ or /O(1)/. The intersection of two sets.
intersection :: IntSet -> IntSet -> IntSet
intersection t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
    | m1 `shorter` m2 = leftiest
    | m2 `shorter` m1 = rightiest
    |     p1 == p2    = binD p1 m1 (intersection l1 l2) (intersection r1 r2)
    |     otherwise   = Nil
  where
    leftiest
      | nomatch p2 p1 m1 = Nil
      |     zero p2 m1   = intersection l1 t2
      |     otherwise    = intersection r1 t2

    rightiest
      | nomatch p1 p2 m2 = Nil
      |     zero p1 m2   = intersection t1 l2
      |     otherwise    = intersection t1 r2

intersection t@(Bin _ _ _ _)   (Tip p bm)    = intersectBM p bm t
intersection t@(Bin _ _ _ _)   (Fin p m)     = intersectFin p m t
intersection   (Bin _ _ _ _)    Nil          = Nil
intersection   (Tip p bm)       t            = intersectBM p bm t
intersection   (Fin p m)        t            = intersectFin p m t
intersection    Nil             _            = Nil


intersectFin :: Prefix -> Mask -> IntSet -> IntSet
intersectFin p1 m1 (Bin p2 m2 l r)
  | undefined = binD p2 m2 (intersectFin p1 m1 l) (intersectFin p1 m1 r)
  | undefined = intersectFin p1 m1 l
  | undefined = intersectFin p1 m1 r
  | undefined = Nil
--  | nomatch
intersectFin p1 m1 (Tip p2 bm2)
  | matchFin p2 p1 m1 = Tip p2 bm2
  |      otherwise    = Nil

-- Fins are never just intersects:
--   * one fin is either subset or superset of the other
--   * or they are disjoint
-- due power of two masks and prefixes
--
intersectFin p1 m1 (Fin p2 m2)
  | finSubsetOf p1 m1 p2 m2 = Fin p2 m2
  | finSubsetOf p2 m2 p1 m1 = Fin p1 m1
  |         otherwise       = Nil
intersectFin _ _    Nil     = Nil

-- not proper subset, just subset of
finSubsetOf :: Prefix -> Mask -> Prefix -> Mask -> Bool
finSubsetOf p1 m1 p2 m2 = (m2 `shorterEq` m1) && matchFin p1 p2 m2


intersectBM :: Prefix -> BitMap -> IntSet -> IntSet
intersectBM p1 bm1 (Bin p2 m2 l r)
  | nomatch p1 p2 m2 = Nil
  |     zero p1 m2   = intersectBM p1 bm1 l
  |      otherwise   = intersectBM p1 bm1 r

intersectBM p1 bm1 (Tip p2 bm2 )
  | p1 == p2  = tipD p1 (bm1 .&. bm2)
  | otherwise = Nil

intersectBM p1 bm1 (Fin p2 m2)
  | matchFin p1 p2 m2 = Tip p1 bm1
  |      otherwise    = Nil

intersectBM _  _    Nil        = Nil


intersections :: [IntSet] -> IntSet
intersections = L.foldl' intersection empty

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}

-- Since difference is no commutative it's simple to match all patterns
-- so WARN dublicated code

infixl 6 `difference`

-- | /O(n + m)/. Find difference of the two sets.
difference :: IntSet -> IntSet -> IntSet
difference t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
    | m1 `shorter` m2 = leftiest
    | m2 `shorter` m1 = rightiest
    |     p1 == p2    = binD p1 m1 (difference l1 l2) (difference r1 r2)
    |    otherwise    = t1
  where
    leftiest
      | nomatch p2 p1 m1 = t1
      |    zero p2 m1    = binD p1 m1 (difference l1 t2) r1
      |     otherwise    = binD p1 m1 l1 (difference r1 t2)

    rightiest
      | nomatch p1 p2 m2 = t1
      |    zero p1 m2    = difference t1 l2
      |     otherwise    = difference t1 r2

difference t1@(Bin _ _ _ _)    (Tip p bm)      = deleteBM p bm t1
difference t1@(Bin _ _ _ _)    (Fin _ _)       = undefined
difference t1@(Bin _ _ _ _)     Nil            = t1
difference t1@(Tip p1 bm )     (Bin p2 m2 _ _) = undefined
difference t1@(Tip p1 bm)      (Fin p2 m2 )    = undefined
difference t1@(Tip p bm)        Nil            = Nil
difference t1@(Fin _ _)     t2@(Bin _ _ _ _)   = undefined
difference t1@(Fin _ _)     t2@(Tip _ _)       = undefined
difference t1@(Fin _ _)     t2@(Fin _ _)       = undefined
difference t1@(Fin _ _)         Nil            = undefined
difference t1@(Tip _ _)         Nil            = t1
difference     Nil              _              = Nil

{--------------------------------------------------------------------
  Min/max
--------------------------------------------------------------------}

-- | /O(min(W, n))/ or /O(1)/. Find minimal element of the set.
--   If set is empty then min is undefined.
--
findMin :: IntSet -> Key
findMin (Bin _ rootM l r)
-- TODO is it correct?
    | rootM < 0 = go r
    | otherwise = go l
  where
    go (Bin _ _ lb _) = go lb
    go (Tip p bm)     = p + findMinBM bm
    go (Fin p _)      = p
    go  Nil           = error "findMax.go: Bin Nil invariant failed"

findMin (Tip p bm) = p + findMinBM bm
findMin (Fin p _)  = p
findMin  Nil       = error "findMin: empty set"

findMinBM :: BitMap -> Int
findMinBM = fromIntegral . trailingZeros
{-# INLINE findMinBM #-}


-- | /O(min(W, n))/ or /O(1)/. Find maximal element of the set.
--  Is set is empty then max is undefined.
--
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
findMaxBM = error "findMaxBM"  --fromIntegral . leadingZeros
{-# INLINE findMaxBM #-}

{--------------------------------------------------------------------
   Map/fold/filter
--------------------------------------------------------------------}
-- TODO fusion
-- | /O(n * min(W, n))/.
--   Apply the function to each element of the set.
--
--   Do not use this operation with the 'universe', 'naturals' or
--   'negatives' sets.
--
map :: (Key -> Key) -> IntSet -> IntSet
map f = fromList . L.map f . toList
{-# INLINE map #-}

listFin :: Prefix -> Mask -> [Key]
listFin p m = [p..(p + m) - 1]

-- | /O(n)/.  Fold the element using the given right associative
--   binary operator.
--
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

-- | /O(n)/. Filter all elements that satisfy the predicate.
--
--   Do not use this operation with the 'universe', 'naturals' or
--   'negatives' sets.
--
filter :: (Key -> Bool) -> IntSet -> IntSet
filter f = go
  where
    go (Bin p m l r) = binD p m (go l) (go r)
    go (Tip p bm) = fromList $ L.filter f $ toList (Tip p bm) -- FIX use foldrBits
    go (Fin p m)  = fromList $ L.filter f $ listFin p m -- FIX fromDistinctAscList
    go  Nil       = Nil

{--------------------------------------------------------------------
  List conversions
--------------------------------------------------------------------}

-- | /O(n * min(W, n))/ or /O(n)/.
--  Create a set from a list of its elements.
--
fromList :: [Key] -> IntSet
fromList = L.foldl' (flip insert) empty

-- | /O(n)/. Convert the set to a list of its elements.
toList :: IntSet -> [Key]
toList = Data.IntSet.Buddy.Internal.foldr (:) []

-- | 'elems' is alias to 'toList' for compatibility.
elems :: IntSet -> [Key]
elems = toList
{-# INLINE elems #-}

{--------------------------------------------------------------------
  Smart constructors
--------------------------------------------------------------------}
-- used when we insert to the tip
tipI :: Prefix -> BitMap -> IntSet
tipI p bm
  | isFull bm = Fin p 64
  | otherwise = Tip p bm
{-# INLINE tipI #-}

-- used when we delete from the tip
tipD :: Prefix -> BitMap -> IntSet
tipD _ 0  = Nil
tipD p bm = Tip p bm
{-# INLINE tipD #-}

-- used when we construct from unknown mask
tip :: Prefix -> BitMap -> IntSet
tip p bm
  |  bm == 0  = Nil
  | isFull bm = Fin p 64
  | otherwise = Tip p bm
{-# INLINE tip #-}

-- used when we insert in left or right subtree
binI :: Prefix -> Mask -> IntSet -> IntSet -> IntSet
-- DONE convert full Tip to Fin, then we can avoid this pattern matching
--binI _ _ (Tip p1 bm1) (Tip p2 bm2)
--  | isFull bm1 && isFull bm2 && xor p1 p2 == 64
--  = Fin p1 128

binI p m (Fin _  m1) (Fin _  m2)
  | m1 == m && m2 == m
-- TODO ?  | m1 == m2
  = Fin p (m * 2)

binI p m l r = Bin p m l r

-- used when we delete from left or right subtree
binD :: Prefix -> Mask -> IntSet -> IntSet -> IntSet
binD _ _ Nil r   = r
binD _ _ l   Nil = l
binD p m l   r  = Bin p m l r
{-# INLINE binD #-}

-- used when we don't know kind of transformation
bin :: Prefix -> Mask -> IntSet -> IntSet -> IntSet
bin _ _ Nil r   = r
bin _ _ l   Nil = l
bin p m (Fin _  m1) (Fin _  m2)
  | m1 == m && m2 == m
-- TODO ?  | m1 == m2
  = Fin p (m * 2)
bin p m l   r  = Bin p m l r
{-# INLINE bin #-}


-- note that join should not merge buddies
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
binCount (Bin _ _ l r) = 1 + binCount l + binCount r
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

origSize :: IntSet -> Int
origSize (Bin _ _ l r) = 5 + origSize l + origSize r
origSize (Tip _ _)     = 3
origSize (Fin _ m)     =
  let tips = m `div` 64
      bins = tips - 1
  in tips * 3 + bins * 5
origSize  Nil          = 1

savedSpace :: IntSet -> Int
savedSpace s = origSize s - wordCount s

bsSize :: IntSet -> Int
bsSize s = findMax s `div` 8

ppStats :: IntSet -> IO ()
ppStats s = do
  putStrLn $ "Bin count: " ++ show (binCount s)
  putStrLn $ "Tip count: " ++ show (tipCount s)
  putStrLn $ "Fin count: " ++ show (finCount s)

  let treeSize = wordCount s
  putStrLn $ "Size in bytes: " ++ show (treeSize * 8)

  let savedSize = savedSpace s
  let bssize    = bsSize s
  let savedSizeBS = bssize - treeSize * 8
  putStrLn $ "Saved space over dense set:  " ++ show (savedSize * 8)
  putStrLn $ "Saved space over bytestring: " ++ show (savedSizeBS)

  let orig = origSize s
  let per   = (fromIntegral savedSize / fromIntegral orig) * (100 :: Double)
  let perBS = (fromIntegral savedSizeBS / fromIntegral bssize) * (100 :: Double)
  putStrLn $ "Percent saved over dense set:  " ++ show per ++ "%"

  putStrLn $ "Percent saved over bytestring: " ++ show perBS ++ "%"

showTree :: IntSet -> String
showTree = go 0
  where
    indent n = replicate (4 * n) ' '
    go n  Nil          = indent n ++ "{}"
    go n (Fin p m)     = indent n ++ show p ++ ".." ++ show (p + m - 1)
    go n (Tip p bm)    = indent n ++ show p ++ " " ++ show bm
    go n (Bin p m l r) = concat
      [ go (succ n) l, "\n"
      , indent n, "+", show p, " ", show m, "\n"
      , go (succ n) r
      ]

showRaw :: IntSet -> String
showRaw = go 0
  where
    indent n = replicate (4 * n) ' '
    go n  Nil          = indent n ++ "Nil"
    go n (Fin p m)     = indent n ++ show p ++ " " ++ show m
    go n (Tip p bm)    = indent n ++ show p ++ " " ++ show bm
    go n (Bin p m l r) = concat
      [ go (succ n) l, "\n"
      , indent n, "+", show p, " ", show m, "\n"
      , go (succ n) r
      ]

putTree :: IntSet -> IO ()
putTree = putStrLn . showTree

putRaw :: IntSet -> IO ()
putRaw = putStrLn . showRaw

{--------------------------------------------------------------------
  Misc
--------------------------------------------------------------------}
foldrBits :: Int -> (Int -> a -> a) -> a -> BitMap -> a
foldrBits p f acc bm = go 0
  where
    go i
      |   i == 64    = acc
      | testBit bm i = f (p + i) (go (succ i))
      |   otherwise  = go (succ i)


isFull :: BitMap -> Bool
isFull x = x == Bits.complement 0
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

-- TODO optimize this
matchFin :: Int -> Prefix -> Mask -> Bool
matchFin i p m = match i p m && (m .&. i == m .&. p)

nomatch :: Int -> Prefix -> Mask -> Bool
nomatch i p m = mask i m /= p
{-# INLINE nomatch #-}

shorter :: Mask -> Mask -> Bool
shorter m1 m2 = natFromInt m1 > natFromInt m2
{-# INLINE shorter #-}

shorterEq :: Mask -> Mask -> Bool
shorterEq m1 m2 = natFromInt m1 >= natFromInt m2
{-# INLINE shorterEq #-}

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
{-
 #if !(defined(__GLASGOW_HASKELL__) && WORD_SIZE_IN_BITS==32)
-}
      x7 = x6 .|. x6 `shiftR` 32
  in x7 `xor` (x7 `shiftR` 1)
{-
 #else
-}
--in x6 `xor` (x6 `shiftRL` 1)
{-
  #endif
-}
{-# INLINE highestBitMask #-}

{--------------------------------------------------------------------
  Big endian operations
--------------------------------------------------------------------}
maskW :: Nat -> Nat -> Prefix
maskW i m = intFromNat (i .&. (Bits.complement (m-1) `xor` m))
{-# INLINE maskW #-}

finMask :: Mask -> Mask
finMask m = m `shiftR` 1
{-# INLINE finMask #-}

{----------------------------------------------------------------------
  Functions that generate Prefix and BitMap of a Key or a Suffix.
----------------------------------------------------------------------}

suffixBitMask :: Int
suffixBitMask = bitSize (undefined :: Word) - 1
{-# INLINE suffixBitMask #-}

prefixBitMask :: Int
prefixBitMask = Bits.complement suffixBitMask
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
