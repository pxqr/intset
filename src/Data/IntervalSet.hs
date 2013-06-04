-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   An efficient implementation of dense integer sets based on
--   Big-Endian PATRICIA trees with buddy suffix compression.
--
--   References:
--
--     * Fast Mergeable Integer Maps (1998) by Chris Okasaki, Andrew Gill
--       <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.5452>
--
--   This implementation is pretty similar to Data.IntSet from
--   containers package. It's expected that Data.IntSet.Buddy will be
--   slightly slower in randomized settings (e.g. fromList
--   [1,3..1000000]) but much faster when we have long sequences
--   (e.g. [1..100000]) up to constant time at many main operations
--   not depending on a set size.
--
--   Conventions in complexity notation:
--
--     * n - number of elements in a set;
--
--     * W - number bits in a 'Key'. This is 32 at 32 bit platforms
--     and 64 at 64 bit platforms;
--
--     * O(n) or O(k) means this operation have complexity O(n) in
--     worst case (e.g. sparse set) or O(k) in best case (e.g. one
--     single interval).
--
--   Note that some operations will take centuries to compute. For
--   exsample @map id universe@ will never end as well as 'filter'
--   applied to 'universe', 'naturals', 'positives' or 'negatives'.
--
--   Also note that some operations like 'union', 'intersection' and
--   'difference' have overriden from default fixity, so use these
--   operations with infix syntax carefully.
--
{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 720
{-# LANGUAGE Safe #-}
#endif

module Data.IntervalSet
       (
         -- * Types
         IntSet(..), Key

         -- * Query
         -- ** Cardinality
       , SB.null
       , size

         -- ** Membership
       , member, notMember

         -- ** Inclusion
       , isSubsetOf, isSupersetOf
       , isProperSubsetOf, isProperSupersetOf

         -- * Construction
       , empty
       , singleton
       , interval

       , naturals
       , negatives
       , universe

         -- * Modification
       , insert
       , delete

         -- * Map Fold Filter
       , SB.map
       , SB.foldr
       , SB.filter

         -- * Splits
       , split, splitGT, splitLT
       , partition

         -- * Min/Max
       , findMin, findMax

         -- * Combine
       , union, unions
       , intersection, intersections
       , difference, symDiff

         -- * Conversion
         -- *** Arbitary
       , elems
       , toList, fromList

         -- *** Ordered
       , toAscList, toDescList
       , fromAscList

#if defined (TESTING)
         -- * Debug
       , isValid, splitFin
#endif
       ) where

import Data.IntervalSet.Internal as SB