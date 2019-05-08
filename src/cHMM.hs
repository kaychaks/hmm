{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
---------------------------------
-- |
-- Module      :  CHMM
-- Copyright   :  (C) Kaushik Chakraborty, 2019
-- License     :  Apache v2 (see the file LICENSE)
-- Maintainer  :  Kaushik Chakraborty <git@kaushikc.org>
-- Stability   :  experimental
--
-- Cost based HMM
-- https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0202337
---------------------------------

module CHMM where

import Papa
import GHC.TypeNats
import Data.Proxy (Proxy(..))
import Linear.V
import Linear
import Combinatorics
import qualified Data.Vector as Vec

import HMM
-----------------------

-- * Documentation

-- | For a specific resiliency property that need evaluation, following algorithm infer the probability of the same property in linear time taking into account corresponding observations and state trajectories
cHMMInfer :: forall s u t b i. (KnownNat s, KnownNat u, KnownNat t, Eq b, Ord b, Integral i, Show i, Show b)
          => HMM s u R b -- ^ HMM model
          -> V t b -- ^ evidence/observation trajectory for time @1 .. t@
          -> V t i -- ^ state trajectory for time @1 .. t@; represented as @0@-based index of state
          -> R -- ^ property probability
cHMMInfer hmm evs xs = (\x y z -> (x * y) / z) <$> y0 <*> y1 <*> y2 $ zs
  where
    y0 :: Vec.Vector (i, b) -> R
    y0 = foldl' (\m (s,o') ->
            m *
             sensorDiagonal hmm o' ^?! _Just . ix (fromIntegral s) . ix (fromIntegral s))
            1

    y1 :: Vec.Vector (i, b) -> R
    y1 vs = ifoldl (\idx m (s,_) ->
                   m *
                     bool
                      (hmm ^?! tModel . ix (vs ^?! ix (idx - 1) . _1 . to fromIntegral) . ix (fromIntegral s))
                      ((Linear.transpose (hmm ^. prior) !*! hmm ^. tModel) ^?! ix 0 . ix (fromIntegral s))
                      (idx == 0))
            1 vs

    y2 :: KnownNat s => Vec.Vector (i, b) -> R
    y2 vs =  sum . extract1 $
              ifoldl (\idx m (_,o') ->
                 bool
                     ((sensorDiagonal hmm o' ^?! _Just) !*! Linear.transpose (hmm ^. tModel) !*! m)
                     ((sensorDiagonal hmm o' ^?! _Just) !*! Linear.transpose (hmm ^. tModel) !*! hmm ^. prior)
                     (idx == 0)
              )
              (unitColumn :: Message s R)
              vs

    zs :: Vec.Vector (i, b)
    zs = Vec.zip (toVector xs) (toVector evs)

-- | a trajectory is a sequence of variables which can be random variables (r.v) as part of either sensor or transition model of a HMM or values from the support of some r.v.
trajectory :: forall n t a . (KnownNat n, KnownNat t)
           => Proxy t -- ^ choice parameter
           -> V n a -- ^ superset of choice variables which is divided into sets each of length provided by the choice parameter
           -> V (n ^ t) (V t a) -- ^ final set with @n^t@ ordered subsets each of length @t@
trajectory p = V . Vec.fromList . fmap (V . Vec.fromList) . variateRep t' . toList
                   where
                     t' = fromIntegral $ natVal p

-- | HMM model representing the swarm robotics use case of the paper
--
-- Transition Model
--
-- @
-- [0.7393859 , 0.2233470 , 0.0330670 , 0.0042001]
-- [0.0928202 , 0.7172515 , 0.1719556 , 0.0179727]
-- [0.0139341 , 0.1755173 , 0.6936519 , 0.1168966]
-- [0.0047083 , 0.0403520 , 0.2561893 , 0.6987504]
-- @
--
-- Sensor Model
--
-- @
-- [1.00000,  0.00000,   0.00000,   0.00000]
-- [0.10000,  0.90000,   0.00000,   0.00000]
-- [0.01000,  0.18000,   0.81000,   0.00000]
-- [0.00100,  0.02700,   0.24300,   0.72900]
-- @
--
swarmRobotsHMM :: HMM 4 4 R Int
swarmRobotsHMM  = mkHMM_
                    (Just $ V $ Vec.replicate 4 (toV $ V1 0.25))
                    (V $ Vec.fromList
                      [
                        V $ Vec.fromList [0.7393859 , 0.2233470 , 0.0330670 , 0.0042001],
                        V $ Vec.fromList [0.0928202 , 0.7172515 , 0.1719556 , 0.0179727],
                        V $ Vec.fromList [0.0139341 , 0.1755173 , 0.6936519 , 0.1168966],
                        V $ Vec.fromList [0.0047083 , 0.0403520 , 0.2561893 , 0.6987504]
                      ])
                    (V $ Vec.fromList [1..4])
                    (Linear.transpose (V $ Vec.fromList
                      [
                        V $ Vec.fromList [1.00000,  0.00000,   0.00000,   0.00000],
                        V $ Vec.fromList [0.10000,  0.90000,   0.00000,   0.00000],
                        V $ Vec.fromList [0.01000,  0.18000,   0.81000,   0.00000],
                        V $ Vec.fromList [0.00100,  0.02700,   0.24300,   0.72900]
                      ]))

-- | inferring the /l-resistance/ property of the swarm robots as represented in the 'swarmRobotsHMM' model given a series of observation and a paticular /l/ value to check against
-- 
-- E.g.
--
-- >>>  runCHMMInfer [3,2,3] 2
-- 0.7756162284376005
--
runCHMMInfer :: [Int] -> Int -> R
runCHMMInfer evs l = let v = Vec.fromList [0 .. dim (swarmRobotsHMM ^. tModel) - 1]
                         n = length evs
                     in
                       reifyDimNat n $ \t' ->
                                         reifyVectorNat v $ \n' ->
                                                              let ts = trajectory t' n'
                                                                  valid_ts = V $ Vec.filter (Vec.all (>=l) . toVector) $ toVector ts
                                                              in
                                                                sum $ cHMMInfer swarmRobotsHMM (V $ Vec.fromList evs) <$> valid_ts






