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
---------------------------------

module CHMM where

import Papa
import GHC.TypeNats
import Data.Proxy (Proxy(..))
import Linear.V
import Combinatorics
import qualified Data.Vector as Vec

import HMM
-----------------------

trajectory :: forall n t a . (KnownNat n, KnownNat t) => Proxy t -> V n a -> V (n ^ t) (V n a)
trajectory p = V . Vec.fromList . fmap (V . Vec.fromList) . variateRep t' . toList
                   where
                     t' = fromIntegral $ natVal p

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






