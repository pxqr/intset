-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  little endian arch
--
--   Fast conversion from or to lazy and strict bytestrings.
--   Serialized IntSets are represented as single continious bitmap.
--
--   This module is kept separated due safe considerations.
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.IntervalSet.ByteString
       ( fromByteString
       , toByteString
       ) where

import Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Foreign

import Data.IntervalSet.Internal as S


#if defined(__GLASGOW_HASKELL__)
#include "MachDeps.h"
#endif

{-
  it seems like we have this conversion hella fast by desing
  e.g. read by blocks(bitmaps), fast merge, fast 'bin'

  but we need to make memory access patterns linear and dense
  e.g. read left subtree /before/ right subtree;
  TODO carefully force this behaviour
-}

-- | Unpack 'IntSet' from bitmap.
fromByteString :: ByteString -> IntSet
fromByteString bs =
    let (fptr, off, len) = BS.toForeignPtr bs in
    BS.inlinePerformIO $ withForeignPtr fptr $ \_ptr -> do
      let ptr = _ptr `advancePtr` off
      let !s = goFrom (castPtr ptr) len
      return $! s
  where
    wordSize = sizeOf (0 :: Word)

    goFrom ptr len = go 0 empty -- goTree 0 len
      where
        go :: Int -> IntSet -> IntSet
        go !x !acc
          |  x + wordSize <= len  = do
            let !bm = BS.inlinePerformIO (peekByteOff ptr x) -- TODO read little endian
            let !s  = unionBM (x * wordSize) bm acc
            go (x + wordSize) s
          | otherwise = goBytes x acc

        -- normally this loop should run only at the mostleft region of bitmap
        -- note that the left index is not necessary multiple of a word size
        goBytes :: Int -> IntSet -> IntSet
        goBytes !i !s
          |   i < len =
            let wbm = BS.inlinePerformIO (peekByteOff ptr i)
                s'  = foldrWord (i * 8) insert s wbm
            in  goBytes (i + 1)  s'
          | otherwise = s

{-
        goTree :: Int -> Int -> IntSet
        goTree !l !r
          | traceShow (l, r) False = undefined
          | r - l > wordSize =
            let !px  = l `shiftL` 3
                !qx  = r `shiftL` 3
                !msk = branchMask px qx
                -- TODO fix mid
                !mid = let br = branchMask l r in if br == r
                                                  then div (r + l) 2
                                                  else br
            in traceShow (l, mid, r, px, qx, msk) $
               bin  px msk (goTree l mid) (goTree mid r)

          | r - l == wordSize =
            let bm = BS.inlinePerformIO (peekByteOff ptr l)
            in tip (l * wordSize) bm

          | otherwise = goBytes l r empty
-}

foldrWord :: Int -> (Int -> a -> a) -> a -> Word8 -> a
foldrWord p f acc bm = go 0
  where
    go i
      |    i == 8    = acc
      | testBit bm i = f (p + i) (go (succ i))
      |   otherwise  = go (succ i)
{-
-- | Pack 'IntSet' as bitmap to the bytestring builder.
--
--   NOTE: negative elements are ignored!
--
toBuilder :: IntSet -> Builder
toBuilder _s = go (splitGT (-1) _s) (\_ -> BS.byteString "") 0
  where
    indent n p = BS.byteString $ BS.replicate ((p - n) `shiftR` 3) 0
    {-# INLINE indent #-}

    {-# INLINE wordLE #-}
#if WORD_SIZE_IN_BITS == 64
    wordLE = BS.word64LE
#elif WORD_SIZE_IN_BITS == 32
    wordLE = BS.word32LE
#else
#error Unsupported platform
#endif
    -- TODO preallocate buffer and write
    -- TODO trim last zeroed bytes
    go :: IntSet -> (Int -> Builder) -> (Int -> Builder)
    go s c !n = case s of
      Bin _ _ l r -> go l (go r c) n
      Tip p bm    -> indent n p <>
                     wordLE (fromIntegral bm) <>
                     c (p + WORD_SIZE_IN_BITS)
      Fin p m     -> indent n p <>
                     BS.byteString (BS.replicate (m `shiftR` 3) 255) <>
                     c (p + m)
      Nil         -> c n

-- | Pack the 'IntSet' as bitmap to the lazy bytestring.
--
--   NOTE: you should prefer 'toLazyByteString' over 'toByteString'.
--
--   NOTE: negative elements are ignored!
--
toLazyByteString :: IntSet -> BSL.ByteString
toLazyByteString = BS.toLazyByteString . toBuilder
{-# INLINE toLazyByteString #-}
-}
-- | Pack the 'IntSet' as bitmap to the strict bytestring.
--
--   NOTE: negative elements are ignored!
--
toByteString :: IntSet -> ByteString
toByteString snp =
    let s        = splitGT (-1) snp
        maxEl    = if S.null snp then 0 else findMax s + 1
        sizeWord = wordSize maxEl
        sizeByte = byteSize maxEl
    in BS.take sizeByte $
       BS.unsafeCreate (sizeWord * sizeOf (undefined :: BitMap))
                       (`start` s) -- createAndTrim
  where
    wordSize x = (x `div` WORD_SIZE_IN_BITS) +
                 if (x `mod` WORD_SIZE_IN_BITS) == 0 then 0 else 1
    byteSize x = (x `div` 8)  + if (x `mod` 8)  == 0 then 0 else 1

    indent :: Ptr Word8 -> Int -> Int -> IO ()
    indent ptr n p = void $ BS.memset (ptr `plusPtr`  shiftR n 3) 0
                                      (fromIntegral  (shiftR (p - n) 3))
    {-# INLINE indent #-}

    start :: Ptr Word8 -> IntSet -> IO ()
    start ptr s = void $ write s 0
      where
        write :: IntSet -> Int -> IO Int
        write s' !n = case s' of
          Bin _ _ l r -> write l n >>= write r
          Tip p bm    -> do
            indent ptr n p
            pokeByteOff ptr (p `shiftR` 3) bm
            return (p + WORD_SIZE_IN_BITS)

          Fin p m     -> do
            indent ptr n p
            _ <- BS.memset (ptr `advancePtr` shiftR p 3) 255
                           (fromIntegral    (shiftR m 3))
            return (p + m)

          Nil         -> return n
