-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Fast conversion from or to lazy and strict bytestrings.
--   This module is kept separated due safe considerations.
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.IntSet.Buddy.ByteString
       (
         fromByteString, toByteString
       ) where

import Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import           Data.ByteString.Builder  (Builder)
import qualified Data.ByteString.Builder  as BS
import qualified Data.ByteString.Lazy  as BSL
import Foreign
import Data.Monoid

import Data.IntSet.Buddy.Internal

{-
  it seems like we have this conversion hella fast by desing
  e.g. read by blocks(bitmaps), fast merge, fast 'bin'
  but we need to make memory access patterns linear

-}

fromByteString :: ByteString -> IntSet
fromByteString bs =
    let (fptr, off, len) = BS.toForeignPtr bs in
    BS.inlinePerformIO $ withForeignPtr fptr $ \_ptr -> do
      let ptr = _ptr `advancePtr` off
      let !s = goFrom (castPtr ptr) len
      return $! s
  where
    goFrom ptr len =
--      go 0 empty
        goTree 0 len
      where
        -- TODO go treelike
        wordSize = sizeOf (0 :: Word)

        go :: Int -> IntSet -> IntSet
        go !x !acc
          |  x + wordSize <= len  = do
            let bm = BS.inlinePerformIO (peekByteOff ptr x)
            let !s = unionBM (x * 8) bm acc
            go (x + wordSize) s
          | otherwise = goBytes x acc

        goTree :: Int -> Int -> IntSet
        goTree !l !r
          | r - l <= wordSize =
            let bm = BS.inlinePerformIO (peekByteOff ptr l)
            in tip (l * wordSize) bm
          | otherwise =
            let !mid = l + ((r - l) `shiftR` 1)
                !px  = l `shiftL` 3
                !msk = branchMask px (mid `shiftL` 3)
            in bin  px msk (goTree l mid) $! (goTree mid r)

        goBytes :: Int -> IntSet -> IntSet
        goBytes !x !s | x == len = s
        goBytes  _  _ = error "not implemented"

-- TODO split by 0 before building bytestring, such that we throw away negative ints

toBuilder :: IntSet -> Builder
toBuilder = snd . go 0
  where
    indent n p = BS.byteString $ BS.replicate (div (p - n) 8) 0

    -- TODO check for negative
--    go n (Bin _ _ Nil r) -- etc
    go n (Bin _ _ l r) = let (n',  bl) = go n l
                             (n'', br) = go n' r
                         in (n'', bl <> br)
    go n (Tip p bm)    = (p + 8, indent n p <> BS.word64LE (fromIntegral bm))
    go n (Fin p m)     = (p + m, indent n p <>
                                 BS.byteString (BS.replicate (div m 8) 255))
    go _  Nil          = (0, BS.byteString "")

toLazyByteString :: IntSet -> BSL.ByteString
toLazyByteString = BS.toLazyByteString . toBuilder
{-# INLINE toLazyByteString #-}

toByteString :: IntSet -> ByteString
toByteString = BSL.toStrict . toLazyByteString
{-# INLINE toByteString #-}
