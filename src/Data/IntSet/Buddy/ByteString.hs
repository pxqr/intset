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
module Data.IntSet.Buddy.ByteString
       (
         fromByteString, toByteString
       ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Foreign

import Data.IntSet.Buddy.Internal


fromByteString :: ByteString -> IntSet
fromByteString bs =
    let (fptr, off, len) = B.toForeignPtr bs in
    B.inlinePerformIO $ withForeignPtr fptr $ \_ptr -> do
      let ptr = _ptr `advancePtr` off
      goFrom (castPtr ptr) len
  where
    goFrom ptr len = go 0 empty
      where
        go x !acc
          |  x < len  = do
            e <- peekByteOff ptr x
            let s = tip (x * 8) e
            go (x + sizeOf (0 :: Word)) (union s acc)
          | otherwise = return acc

toByteString :: IntSet -> ByteString
toByteString = error "toByteString: not implemented"

{-
fromWord1 :: Int -> Word8 -> IntSet
fromWord1 n 1 = Tip 1 n
fromWord1 _ _ = Nil
{-# INLINE fromWord1 #-}

fromWord2 :: Int -> Word8 -> IntSet
fromWord2 n 3 = Tip 2 n
fromWord2 n w = fromWord1 n       (w .&. 0x1)
             <> fromWord1 (n + 1) (w `shiftR` 1)
{-# INLINE fromWord2 #-}

fromWord4 :: Int -> Word8 -> IntSet
fromWord4 n 15 = Tip 4 n
fromWord4 n w  = fromWord2 n       (w .&. 0x3)
              <> fromWord2 (n + 2) (w `shiftR` 2)
{-# INLINE fromWord4 #-}

fromWord8 :: Int -> Word8 -> IntSet
fromWord8 n 255 = Tip 8 n
fromWord8 n w   = fromWord4 n       (w .&. 0x15)
               <> fromWord4 (n + 4) (w `shiftR` 4)
{-# INLINE fromWord8 #-}

fromWord16 :: Int -> Word16 -> IntSet
fromWord16 n 65535 = Tip 16 n
fromWord16 n w     = fromWord8 n       (fromIntegral (w .&. 0x255))
                  <> fromWord8 (n + 4) (fromIntegral (w `shiftR` 8))
{-# INLINE fromWord16 #-}

fromWord32 :: Int -> Word32 -> IntSet
fromWord32 n 65535 = Tip 32 n
fromWord32 n w     = fromWord16  n       (fromIntegral (w .&. 0x255))
                  <> fromWord16 (n + 16) (fromIntegral (w `shiftR` 8))
{-# INLINE fromWord32 #-}

fromWord64 :: Int -> Word64 -> IntSet
fromWord64 n 65535 = Tip 64 n
fromWord64 n w     = fromWord16  n       (fromIntegral (w .&. 0x255))
                  <> fromWord16 (n + 16) (fromIntegral (w `shiftR` 8))
{-# INLINE fromWord64 #-}

fromByteString :: ByteString -> IntSet
fromByteString = snd . B.foldr ins (0, empty)
  where

    ins w (!n, !s) = (n + 8, s `union` fromWord8 n w)

toByteString :: IntSet -> ByteString
toByteString = undefined
-}