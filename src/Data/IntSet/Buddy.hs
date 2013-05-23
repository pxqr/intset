-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   An efficient implementation of dense integer sets based on
--   Big-Endian PATRICIA tree with buddy suffix compression.
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
--   (e.g. [1..100000]) up to constant time at many main operations.
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
--   -- TODO Fin explanation
--   -- TODO move to Internal.hs
--
{-# LANGUAGE CPP #-}
module Data.IntSet.Buddy
       (
         -- * Types
         IntSet, Key

         -- * Query
       , SB.null
       , size
       , member, notMember

         -- * Construction
       , empty
       , singleton
       , insert

         -- * Map/Fold/Filter
       , SB.map
       , SB.foldr
       , SB.filter

         -- * Combine
       , union, unions

         -- * Conversion
         -- ** Lists
       , elems
       , toList, fromList

#if defined (TESTING)
         -- * Debug
       , isValid
#endif
       ) where

import Data.IntSet.Buddy.Internal as SB