{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module HMM where

-- import Prelude (undefined)
-- import System.IO (print)
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

-- | Type synonyms
type R = Double
type TransitionModel (s :: Nat) a = V s (V s a)
type SensorDiagonal (s  :: Nat) a = V s (V s a)
type SensorModel (t :: Nat) (s :: Nat) a = V t (SensorDiagonal s a)
type Message (s :: Nat) a = V s (V 1 a)
type Distribution (s :: Nat) a = V s a

-- | Utility Functions

-- | Multiply each number by a constant such that the sum is 1.0

normalise :: (Fractional a, Foldable f, Functor f) => f a -> f a
normalise xs | null xs = xs
             | otherwise =
                 let
                   s = sum xs
                 in
                   (/ s) <$> xs

toHM :: (KnownNat s, KnownNat t) => V s (V t R) -> HM.L s t
toHM = HM.matrix . foldMap (Vec.toList . toVector)

fromHM :: (KnownNat s, KnownNat t) => HM.L s t -> V s (V t R)
fromHM m = V $ V <$> Vec.fromList (fmap Vec.fromList $ HMat.toLists $ HM.extract m)

inverse :: forall s. (KnownNat s) => V s (V s R) -> V s (V s R)
inverse  = fromHM . HM.inv . toHM

reverseV :: forall s a. (KnownNat s) => V s a -> V s a
reverseV = V . Vec.fromList . Papa.reverse . toList

extract1 :: (KnownNat s) => V s (V 1 a) -> V s a
extract1 =  V . foldMap toVector

infix 8 !**!
(!**!) :: (KnownNat m, KnownNat n) => V m (V n R) -> V m (V n R) -> V m (V n R)
as !**! bs = fromHM $ toHM as * toHM bs

unitColumn :: forall s. KnownNat s => Message s R
unitColumn = V $ Vec.replicate (fromIntegral $ natVal (Proxy :: Proxy s))  (toV $ V1 1.0)


-- | Hidden Markov Models

data HMM (s :: Nat) (t :: Nat)  a b = HMM {
  -- prior distribution which is used as initial forward message
  _prior :: Message s a,
  -- transition model with `s` states as `sxs` matrix 
  _tModel :: TransitionModel s a,
  -- evidence value vector, each index maps to the corresponding evidence values
  _sDist :: V t b,
  -- sensor model with `t` evidence values having `sxs` diagonal matrix capturing their ditributions for each state `s`
  _sModel :: SensorModel t s a
  } deriving (Show)
makeLenses ''HMM


-- | Smart HMM constructors

-- | Make HMM
-- `mp :: Maybe (Message s a)` -- Prior distribution on the initial state, `P(X0)`. If nothing then considered `(0.5,0.5)`
-- `xs :: TransitionModel s a` -- Transition Model as `sxs` matrix, `s` being the number of states
-- `evs :: V t b` - evidence values vector map
-- `es :: V t (V s a)` -- a `txs` matrix for each evidence value `t`, a vector capturing conditional probabilities for each state `s`
mkHMM_ :: (KnownNat s, KnownNat t) => Maybe (Message s R) -> TransitionModel s R -> V t b -> V t (V s R) -> HMM s t R b
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

sensorDiagonal :: (KnownNat s, KnownNat t, Eq b) => HMM s t a b -> b -> Maybe (SensorDiagonal s a)
sensorDiagonal hmm e = findIndexOf (sDist.folded) (== e) hmm >>= (\i -> hmm ^? sModel . ix i)

-- | Fixed Lag Smoothing

-- | Persistent State
data Persistent (s :: Nat) a b d= Persistent {
  _t :: d,
  _f_msg :: Message s a,
  _b :: V s (V s a),
  _e_td_t :: Vec.Vector b
  } deriving (Show)
makeLenses ''Persistent

-- | State to start with
persistentInit :: forall s b d. (KnownNat s, Integral d) => Message s R -> Persistent s R b d
persistentInit p = Persistent { _t = 1, _f_msg = p, _b = identity, _e_td_t = Vec.empty}

-- | Filtering message propagated forward
forward :: (KnownNat s) => HMM s t R b -> SensorDiagonal s R  -> Message s R -> Message s R
forward hmm ot f = normalise (ot !*! Linear.transpose (hmm ^. tModel) !*! f)

-- | Smoothing message propagated backward
backward :: (KnownNat s) => HMM s t R b -> SensorDiagonal s R -> Message s R -> Message s R
backward hmm ok1 bk2 = (hmm ^. tModel) !*! ok1 !*! bk2


-- | Online Algorithm for smoothing with a fixed time lag of `d` steps
-- hmm -- HMM model
-- d -- length of lag
-- e -- evidence at time t
fixedLagSmoothing :: forall s u b d. (KnownNat s, KnownNat u, Eq b, Integral d) => HMM s u R b -> d -> b -> MaybeT (State (Persistent s R b d)) (Distribution s R)
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


-- | The forward–backward algorithm for smoothing: computing posterior prob- abilities of a sequence of states given a sequence of observations
-- hmm - HMM model as a way to implement
-- evs - list of evidences for each time step
forwardBackward :: forall s t u b . (KnownNat t, KnownNat s, KnownNat u, Eq b) => HMM s u R b -> V t b -> V t (Distribution s R)
forwardBackward hmm evs = let
  -- forward messages
  fv :: V (t + 1) (Message s R)
  fv = V $ Vec.fromList $ foldl' (\m@(x:_) e -> forward hmm (sensorDiagonal hmm e ^?! _Just) x : m) [hmm ^. prior] evs
  -- reifying the number of evidences
  tNat = dim evs
  -- getting rid of the prior message from the end of the list
  fv_0 :: V t (Message s R)
  fv_0 = V $ Vec.fromList $ fv ^.. taking tNat traversed
  --  backward messages 
  bs :: V t (Message s R)
  bs = V $ Vec.fromList $ foldl' (\m@(x:_) e -> backward hmm (sensorDiagonal hmm e ^?! _Just) x : m) [unitColumn] $ reverseV evs
  in
    -- smoothing probabilities in reverse order of the list of evidences
    liftA2 (\f b' -> extract1 $ normalise $ f !**! b') fv_0 (reverseV bs)

-- | execution
umbrellaHMM :: HMM 2 2 R Bool
umbrellaHMM = mkHMM1 (V2 (V2 0.7 0.3) (V2 0.3 0.7)) (V2 (V2 0.9 0.2) (V2 0.1 0.8))

runFLSAlgo  :: [Bool] -> [Maybe (Distribution 2 String)]
runFLSAlgo bs = let hmm = mkHMM1 (V2 (V2 0.7 0.3) (V2 0.3 0.7)) (V2 (V2 0.9 0.2) (V2 0.1 0.8))
                    initState = persistentInit (V $ Vec.fromList [V $ Vec.singleton 0.5, V $ Vec.singleton 0.5]) :: Persistent 2 R Bool Integer
                    algo = fixedLagSmoothing hmm 1
                    a :: ([Maybe (Distribution 2 R)], Persistent 2 R Bool Integer)
                    a = foldl' (\(rs, s) x -> runState (runMaybeT $ algo x) s & _1 #%~ ((rs ++) . pure)) ([], initState) bs
                in
                  (a ^. _1) & traverse.traverse.traverse %~ \(x :: R) -> printf "%.3f" x :: String


runFBAlgo :: (KnownNat t) =>V t Bool -> [Distribution 2 R]
runFBAlgo bs = toList $ forwardBackward umbrellaHMM bs
