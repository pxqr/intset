{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Criterion.Main

import Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.IntSet as S
import Data.IntSet.Buddy as SB
import Data.IntSet.Buddy.ByteString as SB
import Data.List as L
import Data.Word




fromByteString :: ByteString -> S.IntSet
fromByteString = S.fromDistinctAscList . indices 0 . B.unpack
  where
    indices _ []       = []
    indices n (w : ws) = wordIxs n w ++ indices (n + 8) ws

    wordIxs n w = L.map ((n +) . fst) $ L.filter snd $ zip [0..] (bits w)

    bits i = L.map (testBit i) [0..bitSize (0 :: Word8) - 1]

main :: IO ()
main = defaultMain
  [ bench "fromList/O-2500"  $ nf S.fromList [0..2500]
  , bench "fromList/O-5000"  $ nf S.fromList [0..5000]
  , bench "fromList/O-10000" $ nf S.fromList [0..10000]
  , bench "fromList/O-20000" $ nf S.fromList [0..20000]
  , bench "fromList/O-20000" $ nf S.fromList (L.map (* 10) [0..20000])
  , bench "fromList/S-2500"  $ nf SB.fromList [0..2500]
  , bench "fromList/S-5000"  $ nf SB.fromList [0..5000]
  , bench "fromList/S-10000" $ nf SB.fromList [0..10000]
  , bench "fromList/S-20000" $ nf SB.fromList [0..20000]
  , bench "fromList/S-20000-sparse" $ nf SB.fromList (L.map (* 10) [0..20000])


  , let !s = S.fromList [1..50000] in
    bench "toList/50000" $ nf S.toList s

  , let !s = SB.fromList [1..50000] in
    bench "toList/50000" $ nf SB.toList s

  , let !bs = B.replicate 10000 255 in
    bench "fromByteString/10000-O" $ whnf Main.fromByteString bs

  , let !bs = B.replicate 1048576 255 in
    bench "fromByteString/8M-S-dense" $ whnf SB.fromByteString bs

--  , let !bs = B.replicate 1048576 85 in
--    bench "fromByteString/8M-S-sparse" $ whnf SB.fromByteString bs

  , let !bs = B.replicate 1048576 0 in
    bench "fromByteString/8M-S-empty" $ whnf SB.fromByteString bs

  , let !s = S.fromList [0..1000000] in
    bench "member/1000000" $ nf (L.all (`S.member` s)) [50000..100000]

  , let !s = SB.fromList [0..1000000] in
    bench "member/1000000" $ nf (L.all (`SB.member` s)) [50000..100000]

--  , bench "distinct/100000/O" $ nf S.fromDistinctAscList  [1..100000]
--  , bench "distinct/20000/S"  $ nf SB.fromDistinctAscList [1..20000]
  ]