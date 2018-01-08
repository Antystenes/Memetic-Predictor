{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module AI.Singularity.Data.Network where
import GHC.TypeLits
import qualified Numeric.LinearAlgebra.Data as LD
import qualified Numeric.LinearAlgebra.Devel as LDev
import qualified Numeric.LinearAlgebra as LA
import qualified Control.Monad.State as S
import Control.Lens
import Data.Proxy
import Data.List (foldl')
import Control.Monad.ST
import System.Random
import Control.Monad
import Control.Arrow
import qualified Data.Vector.Storable as VS

import AI.Singularity.Utils
import AI.Singularity.Data.Matrix
import AI.Singularity.Data.Vector

data GeneralNetwork vec mat inp out where
  FFLayer :: forall vec mat m n. (KnownNat m, KnownNat n) =>
    mat (m+1) n ->
    (forall a. Floating a => a -> a) ->
    GeneralNetwork vec mat (vec m) (vec n)
  FFSeq :: forall vec mat a b c.
    GeneralNetwork  vec mat a b ->
    GeneralNetwork  vec mat b c ->
    GeneralNetwork  vec mat a c
  FromFunc :: (Num a) =>
    (forall b. Num b => b -> b) ->
    GeneralNetwork vec mat a a
  FromBinFunc :: forall a vec mat. Fractional a =>
    (forall b. Fractional b => b -> b -> b) ->
    GeneralNetwork vec mat (a,a) a
  FFDiv :: forall vec mat a' a b b'.
    GeneralNetwork vec mat a b ->
    GeneralNetwork vec mat a' b' ->
    GeneralNetwork vec mat (a, a') (b, b')
  Split :: forall vec mat a b c. Num a =>
    GeneralNetwork vec mat a b ->
    GeneralNetwork vec mat a c ->
    GeneralNetwork vec mat a (b,c)
  SplitNet :: forall vec mat n k l c. (KnownNat n, KnownNat k, KnownNat l, (k + l) ~ n) =>
    GeneralNetwork vec mat (vec n) c ->
    GeneralNetwork vec mat (vec k, vec l) c
  Recurse :: forall vec mat a b. (Num b) =>
    GeneralNetwork vec mat (a,b) b ->
    GeneralNetwork vec mat [a] b
  First :: forall vec mat a b c. (Num c) =>
    GeneralNetwork vec mat a b ->
    GeneralNetwork vec mat (a,c) b
  Second :: forall vec mat a b c. (Num c) =>
    GeneralNetwork vec mat a b ->
    GeneralNetwork vec mat (c,a) b


type Network inp out = GeneralNetwork Vector Matrix inp out

type TrainedNetwork inp out = GeneralNetwork Vector (GeneralInTrainingMat LD.Matrix) inp out

type STNetwork s = GeneralNetwork Vector (STInTrainMat s)

changeMatType :: Applicative f => (forall m n. (KnownNat m, KnownNat n) => mat m n -> f (mat' m n)) ->
                                  GeneralNetwork vec mat inp out -> f (GeneralNetwork vec mat' inp out)
{-# INLINE changeMatType #-}
changeMatType conv (FFLayer m f)    = (\x -> FFLayer x f) <$> conv m
changeMatType conv (FFSeq n1 n2)    = FFSeq <$> changeMatType conv n1 <*> changeMatType conv n2
changeMatType conv (FFDiv n1 n2)    = FFDiv <$> changeMatType conv n1 <*> changeMatType conv n2
changeMatType conv (FromFunc f)     = pure $ FromFunc f
changeMatType conv (FromBinFunc f)  = pure $ FromBinFunc f
changeMatType conv (Split n1 n2)    = Split <$> changeMatType conv n1 <*> changeMatType conv n2
changeMatType conv (SplitNet n1)    = SplitNet <$> changeMatType conv n1
changeMatType conv (Recurse n)      = Recurse <$> changeMatType conv n
changeMatType conv (First n)        = First <$> changeMatType conv n
changeMatType conv (Second n)       = Second <$> changeMatType conv n


matToTrain :: (KnownNat m, KnownNat n) => Matrix m n -> Identity (InTrainMat m n)
matToTrain = pure . (\x -> GeneralInTrainingMat x 0 0)

saveMat :: Matrix m n -> S.State [LD.Matrix Float] (Matrix m n)
saveMat (Mat m) = do
  l <- S.get
  S.put (m:l)
  return (Mat m)

saveWeights = map LD.toLists . reverse . flip S.execState [] . changeMatType saveMat

loadMat :: Matrix m n -> S.State [LD.Matrix Float] (Matrix m n)
loadMat _ = do
  (a:l) <- S.get
  S.put l
  return (Mat a)

loadWeights w = flip S.evalState (map LD.fromLists w) . changeMatType loadMat

toTrain :: Network inp out -> TrainedNetwork inp out
toTrain = runIdentity . changeMatType matToTrain

fromTrain :: TrainedNetwork inp out -> Network inp out
fromTrain = runIdentity . changeMatType (Identity . view trMatr)

toSTTrain :: Network inp out -> ST s (STNetwork s inp out)
toSTTrain = changeMatType toSTInTrainM

instance Show (Network inp out) where
  show (FFLayer m _)     = "weights:\n" ++ show m
  show (FFSeq net1 net2) = show net1 ++ "\n" ++ show net2
  show (FromFunc _ )     = "FromFunc"
  show (FFDiv a b)       = show a ++ "\n\tdiv \n" ++ show b
  show (SplitNet n)      = "\tSplitNet \n" ++ show n
  show (Recurse n)       = "\tRec\n" ++ show n

createLayer :: forall m n. (KnownNat m, KnownNat n) => Matrix (m+1) n -> (forall a. Floating a => a -> a)  -> Network (Vector m) (Vector n)
createLayer weights activator = FFLayer weights activator

randomLayer :: forall m n k. (KnownNat m, KnownNat n, KnownNat k, k ~ (m + 1)) => (forall a. Floating a => a -> a) -> IO (Network (Vector m) (Vector n))
randomLayer activator = (\w -> createLayer w activator) <$> randomMat

identLayer :: Num a => Network a a
identLayer = FromFunc id


generalConductSignal :: forall mat a b. (forall m n. mat m n -> Matrix m n) -> a -> GeneralNetwork Vector mat a b -> b
generalConductSignal _ v (FromFunc f)        = f v
generalConductSignal _ v (FromBinFunc f)     = uncurry f v
generalConductSignal c v (FFSeq net1 net2)   = generalConductSignal c (generalConductSignal c v net1) net2
generalConductSignal c v (FFLayer weights f) = cmap f (c weights #> consV 1 v)
generalConductSignal c v (FFDiv n1 n2)       = (generalConductSignal c (fst v) n1, generalConductSignal c (snd v) n2)
generalConductSignal c v (Split n1 n2)       = (generalConductSignal c v n1, generalConductSignal c v n2)
generalConductSignal c v (SplitNet n)        = generalConductSignal c (uncurry appendV v) n
generalConductSignal c v (First n)           = generalConductSignal c (fst v) n
generalConductSignal c v (Second n)          = generalConductSignal c (snd v) n
generalConductSignal c v (Recurse n)         = foldl' (\b a -> generalConductSignal c (a,b) n) 0 v


conductSignalInTrain = generalConductSignal (view trMatr)

conductSignal :: forall a b. a -> Network a b -> b
conductSignal = generalConductSignal id

runNetwork :: Network a b -> a -> b
runNetwork = flip conductSignal

compose :: Network m n -> Network k m -> Network k n
compose net2 net1 = FFSeq net1 net2

sigmoid :: Floating a => a -> a
sigmoid = (1/) . (1+). exp . negate

softmax :: forall n. KnownNat n => Vector n -> Vector n
softmax = exp >>> id &&& (foldV (+) 0 >>> fromValV) >>> uncurry (/)


atFirst :: Num a => Network b c -> Network (b,a) (c,a)
atFirst = flip FFDiv identLayer
atSecond :: Num a => Network b c -> Network (a,b) (a,c)
atSecond = FFDiv identLayer

takeFst :: (Num c, Num a) => Network (a,c) a
takeFst = First identLayer
takeSnd :: (Num c, Num a) => Network (c,a) a
takeSnd = Second identLayer

prelstm :: forall m n k. (KnownNat m, KnownNat n, KnownNat k) => IO (Network [Vector m] (Vector n, Vector k))
prelstm = do
  let takeInp  = First identLayer
      takeOut  = Second . Second $ identLayer
      -- takeMem  = Second . First $ identLayer
      splitter = Split takeInp takeOut
  x <- SplitNet <$> randomLayer sigmoid
  y <- SplitNet <$> randomLayer sigmoid
  z <- SplitNet <$> randomLayer tanh
  k <- SplitNet <$> randomLayer sigmoid
  l <- randomLayer tanh
  let
    gateCell   = Split (FFSeq splitter x) (Second . First $ identLayer) `FFSeq` FromBinFunc (*)
    secondCell = Split gateCell (splitter `FFSeq` Split y z `FFSeq` FromBinFunc (*)) `FFSeq` FromBinFunc (+)
    thirdCell  = Split secondCell (splitter `FFSeq` k) `FFSeq` Split takeFst (atFirst l `FFSeq` FromBinFunc (*))
      -- get2 pre = Split pre (Split)
  return . Recurse $ thirdCell
-- MnistReader

gru :: forall m n. (KnownNat m, KnownNat n) => IO (Network [Vector m] (Vector n))
gru = do
  z <- SplitNet <$> randomLayer sigmoid
  r <- SplitNet <$> randomLayer sigmoid
  h <- SplitNet <$> randomLayer tanh
  let
    rxt = Split r takeSnd `FFSeq` FromBinFunc (*)
    ht' = Split rxt takeFst `FFSeq` h
    ht  = Split (z `FFSeq` Split (FromFunc (1-)) identLayer) (Split takeSnd ht') `FFSeq` FromBinFunc (*) `FFSeq` FromBinFunc (+)
  return $ Recurse ht
    --beg = Split (Split x takeFst `FFSeq` FromBinFunc (*)) takeFst `FFSeq` z

iden :: forall a. (Num a) => a -> a
iden x = x
