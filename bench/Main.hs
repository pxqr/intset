-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Each function should be benchmarked at least in the following modes:
--
--     * sparse — to see worst case performance. Taking into account
--     both implementations I think [0,64..N] is pretty sparse.
--
--     * dense — to see expected performance. Again [0,2..N] is pretty
--     dense but not interval yet.
--
--     * interval — to see best case performance. Set should be one
--     single interval like [0..N].
--
--   This should help us unify benchmarks and make it more infomative.
--
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
main = defaultMain $
  [ bench "fromList/O-2.5K"  $ nf S.fromList [0..2500]
  , bench "fromList/O-5K"  $ nf S.fromList [0..5000]
  , bench "fromList/O-10K" $ nf S.fromList [0..10000]
  , bench "fromList/O-20K" $ nf S.fromList [0..20000]
  , bench "fromList/O-20K" $ nf S.fromList (L.map (* 10) [0..20000])
  , bench "fromList/S-2.5K"  $ nf SB.fromList [0..2500]
  , bench "fromList/S-5K"  $ nf SB.fromList [0..5000]
  , bench "fromList/S-10K" $ nf SB.fromList [0..10000]
  , bench "fromList/S-20K" $ nf SB.fromList [0..20000]
  , bench "fromList/S-20K-sparse" $ nf SB.fromList (L.map (* 10) [0..20000])


  , let !s = S.fromList [1..50000] in
    bench "toList/50K" $ nf S.toList s

  , let !s = SB.fromList [1..50000] in
    bench "toList/50K" $ nf SB.toList s

  , let !bs = B.replicate 10000 255 in
    bench "fromByteString/10K-O" $ whnf Main.fromByteString bs

  , let !bs = B.replicate 1048576 255 in
    bench "fromByteString/8M-S-dense" $ whnf SB.fromByteString bs

  , let !bs = B.replicate 1048576 0 in
    bench "fromByteString/8M-S-empty" $ whnf SB.fromByteString bs

  , let !s = S.fromList [0..1000000] in
    bench "member/1M" $ nf (L.all (`S.member` s)) [50000..100000]

  , let !s = SB.fromList [0..1000000] in
    bench "member/1M" $ nf (L.all (`SB.member` s)) [50000..100000]
  ]
  ++ concat
  [ splitBenchs
  , splitGTBenchs
  , splitLTBenchs
  , mergeTempl S.union        SB.union        "union"
  , mergeTempl S.intersection SB.intersection "intersection"
  , mergeTempl S.difference   SB.difference   "difference"
  ]


splitBenchs :: [Benchmark]
splitBenchs = complexBench "split" 1000000 (chunk S.split) (chunk SB.split)
  where
    chunk op s = L.foldr ((snd .) . op) s points
    points     = [100,200..1000000]

splitGTBenchs :: [Benchmark]
splitGTBenchs = complexBench "split/GT" 1000000 (chunk S.split) (chunkGT SB.splitGT)
  where
    chunkGT op s = L.foldr op s points
    chunk op s = L.foldr ((snd .) . op) s points
    points     = [100,200..1000000]

splitLTBenchs :: [Benchmark]
splitLTBenchs = complexBench "split/LT" 1000000 (chunk S.split) (chunkGT SB.splitLT)
  where
    chunkGT op s = L.foldr op s points
    chunk op s = L.foldr ((fst .) . op) s points
    points     = reverse [100,200..1000000]

{--------------------------------------------------------------------
  Benchmark Templates
--------------------------------------------------------------------}

type Name = String

type Template =   Name  -- name of benchmark
              ->  Int   -- size of int set
              -> (S.IntSet -> S.IntSet)
              -> (SB.IntSet -> SB.IntSet)
              -> [Benchmark]

type Gen a = Int -> a

genericBench :: Name -> Gen S.IntSet -> Gen SB.IntSet -> Template
genericBench _type genA genB name n f g =
  [ let !s = genA n in bench (name ++ "/O-" ++ show n ++ "-" ++ _type) $ whnf f s
  , let !s = genB n in bench (name ++ "/S-" ++ show n ++ "-" ++ _type) $ whnf g s
  ]

sparseSB :: Gen SB.IntSet
sparseSB n = SB.fromList [0, 64..n * 64 ]

sparseS :: Gen S.IntSet
sparseS n = S.fromDistinctAscList [0, 64..n * 64 ]

denseSB :: Gen SB.IntSet
denseSB n = SB.fromList [0, 2 .. n * 2]

denseS :: Gen S.IntSet
denseS n = S.fromDistinctAscList [0, 2 .. n * 2]

intervalS :: Gen S.IntSet
intervalS n = S.fromDistinctAscList [0..n]

intervalSB :: Gen SB.IntSet
intervalSB n = SB.fromList [0..n]

sparseBench :: Template
sparseBench = genericBench "sparse" sparseS sparseSB

denseBench :: Template
denseBench = genericBench "dense" denseS denseSB

intervalBench :: Template
intervalBench = genericBench "interval" intervalS intervalSB

complexBench :: Template
complexBench name n f g = L.concatMap (\t -> t name n f g) templs
  where
    templs = [sparseBench, denseBench, intervalBench]


mergeTempl :: (S.IntSet  -> S.IntSet  -> S.IntSet)
           -> (SB.IntSet -> SB.IntSet -> SB.IntSet)
           -> String -> [Benchmark]
mergeTempl sop bop n =
  [ let (!a, !b) = (S.fromList [0,64..10000 * 64], S.fromList [1,65..10000 * 64]) in
    bench (n ++"/O-10K-sparse-disjoint")  $ whnf (uncurry sop) (a, b)

  , let (!a, !b) = (S.fromList [0,64..10000 * 64], S.fromList [0,64..10000 * 64]) in
    bench (n ++"/O-10K-sparse-overlap")  $ whnf (uncurry sop) (a, b)

  , let (!a, !b) = (SB.fromList [0,64..10000 * 64], SB.fromList [1,65..10000 * 64]) in
    bench (n ++ "/S-10K-sparse-disjoint") $ whnf (uncurry bop) (a, b)

  , let (!a, !b) = (SB.fromList [0,64..10000 * 64], SB.fromList [0,64..10000 * 64]) in
    bench (n ++ "/S-10K-sparse-overlap") $ whnf (uncurry bop) (a, b)

  , let (!a, !b) = (S.fromList [0,2..500000 * 2], S.fromList [1,3..500000 * 2]) in
    bench (n ++ "/O-500K-dense-disjoint")  $ whnf (uncurry sop) (a, b)

  , let (!a, !b) = (S.fromList [0,2..500000 * 2], S.fromList [0,2..500000 * 2]) in
    bench (n ++ "/O-500K-dense-overlap")  $ whnf (uncurry sop) (a, b)

  , let (!a, !b) = (SB.fromList [0,2..500000 * 2], SB.fromList [1,3..500000 * 2]) in
    bench (n ++ "/S-500K-dense-disjoint") $ whnf (uncurry bop) (a, b)

  , let (!a, !b) = (SB.fromList [0,2..500000 * 2], SB.fromList [0,2..500000 * 2]) in
    bench (n ++ "/S-500K-dense-overlap") $ whnf (uncurry bop) (a, b)

  , let (!a, !b) = (S.fromList [0..500000], S.fromList [0..500000]) in
    bench (n ++ "/O-500K-buddy")  $ whnf (uncurry sop) (a, b)

  , let (!a, !b) = (SB.fromList [0..500000], SB.fromList [0..500000]) in
    bench (n ++ "/S-500K-buddy") $ whnf (uncurry bop) (a, b)
  ]