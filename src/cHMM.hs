{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module CHMM where

import Prelude(undefined)
import Papa

import GHC.TypeNats
import Data.Proxy
import Linear.V
import Combinatorics
import qualified Data.Vector as Vec

-----------------------

trajectory :: forall n t a . (KnownNat n, KnownNat t) => Proxy t -> V n a -> V (n ^ t) (V n a)
trajectory p = V . Vec.fromList . fmap (V . Vec.fromList) . variateRep t . toList
                   where
                     t = fromIntegral $ natVal p
