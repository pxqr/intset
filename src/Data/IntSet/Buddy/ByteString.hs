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

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import           Data.ByteString.Builder  (Builder)
import qualified Data.ByteString.Builder  as BS
import qualified Data.ByteString.Lazy  as BSL
import Foreign
import Data.Monoid

import Data.IntSet.Buddy.Internal


fromByteString :: ByteString -> IntSet
fromByteString bs =
    let (fptr, off, len) = BS.toForeignPtr bs in
    BS.inlinePerformIO $ withForeignPtr fptr $ \_ptr -> do
      let ptr = _ptr `advancePtr` off
      goFrom (castPtr ptr) len
  where
    goFrom ptr len = go 0 empty
      where
        -- TODO go treelike
        go x !acc
          |  x < len  = do
            bm <- peekByteOff ptr x
            let wordSize = sizeOf (0 :: Word)
                -- TODO use tip instead of tipI in insertBM
                -- e.g. insertBMMany
            let s = insertBM (x * wordSize) bm acc
            go (x + wordSize) s
          | otherwise = return acc

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