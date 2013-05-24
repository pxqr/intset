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
