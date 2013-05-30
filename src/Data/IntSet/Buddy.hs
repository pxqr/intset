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
--     * TODO find buddy explanation; if there is no - write somewhere here.
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
--     and 64 at 64 bit platforms
--
--     * O(n) or O(k) means this operation have complexity O(n) in
--     worst case (e.g. sparse set) or O(k) in best case (e.g. one
--     single interval).
--
--   Note that some operations will take centuries to compute. For
--   exsample @map id universe@ will never end as well as filter with
--   conjunction with universe, naturals, positives, negatives.
--
--   Also note that some operations like 'union', 'intersection' and
--   'difference' have overriden from default fixity, so use these
--   operations with conjunction with infix syntax carefully.
--
--   -- TODO Fin explanation
--
{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 720
{-# LANGUAGE Safe #-}
#endif

module Data.IntSet.Buddy
       (
         -- * Types
         IntSet(..), Key

         -- * Query
       , SB.null
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
       , SB.map
       , SB.foldr
       , SB.filter

         -- * Splits
       , split, splitGT, splitLT

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

#if defined (TESTING)
         -- * Debug
       , isValid, splitFin
#endif
       ) where

import Data.IntSet.Buddy.Internal as SB