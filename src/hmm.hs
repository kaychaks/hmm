{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
---------------------------------
-- |
-- Module      :  HMM
-- Copyright   :  (C) Kaushik Chakraborty, 2019
-- License     :  Apache v2 (see the file LICENSE)
-- Maintainer  :  Kaushik Chakraborty <git@kaushikc.org>
-- Stability   :  experimental
--
---------------------------------

module HMM
  (
    HMM(..),
    -- ** Smart Constructors
    mkHMM_, mkHMM, mkHMM1, sensorDiagonal,
    -- * Utility Functions
    normalise, inverse, reverseV, extract1, (!**!), unitColumn,
    -- ** Converting to/from HMatrix matrices
    toHM, fromHM,
    -- * Inference Algorithms
    -- ** Forward-Backward
    forward, backward, forwardBackward,
    -- ** Fixed Lag Smoothing
    fixedLagSmoothing,
    -- * Sample HMM Models
    umbrellaHMM,
    -- * Miscellaneous
    -- ** Type Synonyms
    R, TransitionModel, SensorDiagonal, SensorModel, Message, Distribution,
    -- ** Data Types
    Persistent(..),
    -- ** Test Functions
    runFLSAlgo, runFBAlgo,
    -- ** Lenses
    prior, tModel, sDist, sModel
) where

import Text.Printf (printf)

import Papa
import GHC.TypeNats
import Data.Proxy (Proxy(..))

import Control.Monad.State (State, runState)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

import qualified Data.Vector as Vec

import Linear.V
import Linear

import qualified Numeric.LinearAlgebra.Static as HM
import qualified Numeric.LinearAlgebra as HMat
-----------------------------------------------------------------------------------

type R = Double
type TransitionModel (s :: Nat) a = V s (V s a)
type SensorDiagonal (s  :: Nat) a = V s (V s a)
type SensorModel (t :: Nat) (s :: Nat) a = V t (SensorDiagonal s a)
type Message (s :: Nat) a = V s (V 1 a)
type Distribution (s :: Nat) a = V s a

-- | Hidden Markov Models
data HMM (s :: Nat) (t :: Nat)  a b = HMM {
  -- | prior distribution which is used as initial forward message
  _prior :: Message s a,
  -- | transition model with @s@ states as @sxs@ matrix 
  _tModel :: TransitionModel s a,
  -- | evidence value vector, each index maps to the corresponding evidence values
  _sDist :: V t b,
  -- | sensor model with @t@ evidence values having @sxs@ diagonal matrix capturing their ditributions for each state @s@
  _sModel :: SensorModel t s a
  } deriving (Show)

makeLenses ''HMM

mkHMM_ :: (KnownNat s, KnownNat t) => Maybe (Message s R) -- ^ Prior distribution on the initial state, @P(X0)@. If nothing then considered @(0.5,0.5)@
       -> TransitionModel s R -- ^ Transition Model as @sxs@ matrix, @s@ being the number of states
       -> V t b -- ^ evidence values vector map
       -> V t (V s R) -- ^ a @txs@ matrix for each evidence value @t@, a vector capturing conditional probabilities for each state @s@
       -> HMM s t R b
mkHMM_ mp xs evs = let p = fromMaybe (V $ Vec.replicate (dim xs) 0.5) mp
              in
              HMM p xs evs . (scaled <$>)

-- | A default HMM where both transition model states and evidence variables have boolean support
mkHMM :: KnownNat s => TransitionModel s R -> V 2 (V s R) -> HMM s 2 R Bool
mkHMM ts = mkHMM_ Nothing ts (V $ Vec.fromList [True, False])

-- | A default HMM with one state variable having boolean support
mkHMM1 :: V2 (V2 R) -> V2 (V2 R) -> HMM 2 2 R Bool
mkHMM1 ts ss = let f = (_V #) . over mapped toV
               in
                 uncurry mkHMM $ over both f (ts , ss)

-- | create a @sxs@ diagonal matrix with corresponding posterior probabilities from the sensor model of the @HMM@ for an input sensor value
sensorDiagonal :: (KnownNat s, KnownNat t, Eq b)
               => HMM s t a b
               -> b -- ^ sensor value
               -> Maybe (SensorDiagonal s a)
sensorDiagonal hmm e = findIndexOf (sDist.folded) (== e) hmm >>= (\i -> hmm ^? sModel . ix i)



-- | Multiply each number by a constant such that the sum is 1.0
normalise :: (Fractional a, Foldable f, Functor f) => f a -> f a
normalise xs | null xs = xs
             | otherwise =
                 let
                   s = sum xs
                 in
                   (/ s) <$> xs

-- | inverse a square matrix
inverse :: forall s. (KnownNat s) => V s (V s R) -> V s (V s R)
inverse  = fromHM . HM.inv . toHM

-- | reverse contents of a 'Linear.V' vector
reverseV :: forall s a. (KnownNat s) => V s a -> V s a
reverseV = V . Vec.fromList . Papa.reverse . toList

extract1 :: (KnownNat s) => V s (V 1 a) -> V s a
extract1 =  V . foldMap toVector

-- | Pointwise product of 2 @mxn@ 'V' vectors
infix 8 !**!
(!**!) :: (KnownNat m, KnownNat n) => V m (V n R) -> V m (V n R) -> V m (V n R)
as !**! bs = fromHM $ toHM as * toHM bs

-- | an unit vector having @s@ columns
unitColumn :: forall s. KnownNat s => Message s R
unitColumn = V $ Vec.replicate (fromIntegral $ natVal (Proxy :: Proxy s))  (toV $ V1 1.0)

-- | take a @sxt@ 'Linear.V.V' matrix and give corresponding 'Numeric.LinearAlgebra.Static.L' version
toHM :: (KnownNat s, KnownNat t) => V s (V t R) -> HM.L s t
toHM = HM.matrix . foldMap (Vec.toList . toVector)

-- | take a @sxt@ 'Numeric.LinearAlgebra.Static.L' matrix and give corresponding 'Linear.V.V' version
fromHM :: (KnownNat s, KnownNat t) => HM.L s t -> V s (V t R)
fromHM m = V $ V <$> Vec.fromList (fmap Vec.fromList $ HMat.toLists $ HM.extract m)

-- | Filtering message propagated forward
forward :: (KnownNat s) => HMM s t R b -> SensorDiagonal s R  -> Message s R -> Message s R
forward hmm ot f = normalise (ot !*! Linear.transpose (hmm ^. tModel) !*! f)

-- | Smoothing message propagated backward
backward :: (KnownNat s) => HMM s t R b -> SensorDiagonal s R -> Message s R -> Message s R
backward hmm ok1 bk2 = (hmm ^. tModel) !*! ok1 !*! bk2

-- | The forward–backward algorithm for smoothing: computing posterior prob- abilities of a sequence of states given a sequence of observations
forwardBackward :: forall s t u b . (KnownNat t, KnownNat s, KnownNat u, Eq b)
                => HMM s u R b -- ^ HMM model as a way to implement
                -> V t b -- ^ list of evidences for each time step
                -> V t (Distribution s R)
forwardBackward hmm evs = let
  -- reifying the number of evidences
  tNat = dim evs
  -- forward messages from time t .. 0
  fv :: V (t + 1) (Message s R)
  fv = V $ Vec.fromList $ foldl' (\m@(x:_) e -> forward hmm (sensorDiagonal hmm e ^?! _Just) x : m) [hmm ^. prior] evs
  -- getting rid of the prior message from the end of the list
  -- so now forward messages vector is from time t .. 1
  fv_0 :: V t (Message s R)
  fv_0 = V $ Vec.fromList $ fv ^.. taking tNat traversed

  --  backward messages from time 1 .. t
  bs :: V t (Message s R)
  bs = V $ Vec.fromList $ foldl' (\m@(x:_) e -> backward hmm (sensorDiagonal hmm e ^?! _Just) x : m) [unitColumn] $ reverseV evs
  -- reversing the backward messages
  -- so now the backward messages vector is from time t .. 1
  revBs :: V t (Message s R)
  revBs = reverseV bs
  in
    -- smoothing probabilities in reverse order of the list of evidences
    -- i.e. starting from time t .. 1
    liftA2 (\f b' -> extract1 $ normalise $ f !**! b') fv_0 revBs


-- | Persistent State
data Persistent (s :: Nat) a b d= Persistent {
  _t :: d, -- ^ current time
  _f_msg :: Message s a, -- ^ the forward message @P(Xt|e1:t)@
  _b :: V s (V s a), -- ^ the @d@-step backward transformation matrix
  _e_td_t :: Vec.Vector b -- ^ double-ended list of evidence from @t − d@ to @t@
  } deriving (Show)

makeLenses ''Persistent

-- | Initial persistent state where
--
--  * t = 1
--
--  * f_msg  = hmm.prior
--
--  * b = identity matrix
--
--  * e_td_t = empty vector
persistentInit :: forall s b d. (KnownNat s, Integral d) => Message s R -> Persistent s R b d
persistentInit p = Persistent { _t = 1, _f_msg = p, _b = identity, _e_td_t = Vec.empty}

-- | Online Algorithm for smoothing with a fixed time lag of @d@ steps
fixedLagSmoothing :: forall s u b d. (KnownNat s, KnownNat u, Eq b, Integral d)
                  => HMM s u R b -- ^ HMM model
                  -> d -- ^ length of lag
                  -> b -- ^ evidence at time @t@
                  -> MaybeT (State (Persistent s R b d)) (Distribution s R)
fixedLagSmoothing hmm d e = do
  e_td_t %= flip Vec.snoc e

  o_t <- uses e_td_t $ (^?! _Just) . sensorDiagonal hmm . Vec.last

  t' <- use t
  if t' > d then
    do
      e_td_t %= Vec.drop 1
      o_tmd <- uses e_td_t $ (^?! _Just) . sensorDiagonal hmm . Vec.head
      f_msg %= forward hmm o_tmd
      b %= \b' -> inverse o_tmd !*! inverse (hmm ^. tModel) !*! b' !*! (hmm ^. tModel) !*! o_t
      t += 1

      (f'', b'') <- liftA2 (,) (use f_msg) (use b)

      return $ extract1 $ normalise (f'' !**! (b'' !*! unitColumn))
  else
    do
      b %= \b' -> b' !*! (hmm ^. tModel) !*! o_t
      t += 1
      mzero


umbrellaHMM :: HMM 2 2 R Bool
umbrellaHMM = mkHMM1 (V2 (V2 0.7 0.3) (V2 0.3 0.7)) (V2 (V2 0.9 0.2) (V2 0.1 0.8))


runFLSAlgo  :: [Bool] -> Integer -> [Maybe (Distribution 2 String)]
runFLSAlgo bs d = let hmm = mkHMM1 (V2 (V2 0.7 0.3) (V2 0.3 0.7)) (V2 (V2 0.9 0.2) (V2 0.1 0.8))
                      initState = persistentInit (V $ Vec.fromList [V $ Vec.singleton 0.5, V $ Vec.singleton 0.5]) :: Persistent 2 R Bool Integer
                      algo = fixedLagSmoothing hmm d
                      a :: ([Maybe (Distribution 2 R)], Persistent 2 R Bool Integer)
                      a = foldl' (\(rs, s) x -> runState (runMaybeT $ algo x) s & _1 #%~ ((rs ++) . pure)) ([], initState) bs
                  in
                    (a ^. _1) & traverse.traverse.traverse %~ \(x :: R) -> printf "%.3f" x :: String


runFBAlgo :: [Bool] -> [Distribution 2 R]
runFBAlgo bs = reifyVectorNat (Vec.fromList bs) (toList . forwardBackward umbrellaHMM)
