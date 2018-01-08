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
module AI.Singularity.Data.Vector where
import GHC.TypeLits
import qualified Numeric.LinearAlgebra.Data as LD
import qualified Numeric.LinearAlgebra.Devel as LDev
import qualified Numeric.LinearAlgebra as LA
import Control.Lens
import Data.Proxy
import Control.Monad.ST
import System.Random
import Control.Monad
import Control.Arrow
import qualified Data.Vector.Storable as VS

import AI.Singularity.Utils
import AI.Singularity.Data.Matrix

data GeneralVector :: (* -> *) -> Nat -> * where
  Vec :: vec Float -> GeneralVector vec n
deriving instance Show (vec Float) => Show (GeneralVector vec n)

vec :: Lens' (GeneralVector vec n) (vec Float)
{-# INLINE vec #-}
vec f (Vec v) = Vec <$> f v

type Vector = GeneralVector LD.Vector

type MutVector s = GeneralVector (LDev.STVector s)


thawV :: Vector n -> ST s (MutVector s n)
thawV = Vec <$< LDev.thawVector . view vec


cmap :: (Float -> Float) -> Vector n -> Vector n
cmap = over vec . LD.cmap

foldV :: KnownNat n => (Float -> Float -> Float) -> Float -> Vector n -> Float
foldV f z = VS.foldr f z . view vec

lenV :: forall n. KnownNat n => Vector n -> Int
lenV _ = natToInt @ n

fromListV :: forall n. KnownNat n => [Float] -> Vector n
fromListV = Vec . LD.fromList . take l . cycle
  where l = natToInt @ n

fromValV :: forall n. KnownNat n => Float -> Vector n
fromValV = fromListV . repeat

toListV :: forall n. KnownNat n => Vector n -> [Float]
toListV (Vec v) = VS.toList v

consV :: forall n. (KnownNat n) => Float -> Vector n -> Vector (n+1)
consV d = Vec . VS.cons d . view vec

tailV :: forall n. (KnownNat n) => Vector (n+1) -> Vector n
tailV = Vec . VS.tail . view vec

takeV :: forall n m. (KnownNat n, KnownNat m, m <= n) => Vector n -> Vector m
takeV = Vec . VS.take m . view vec
  where m = natToInt @ n

dropV :: forall n m. (KnownNat n, KnownNat m, m <= n) => Vector n -> Vector (n - m)
dropV = Vec . VS.drop m . view vec
  where m = natToInt @ m

splitV :: forall k l n. (KnownNat k, KnownNat l, KnownNat n, (k+l) ~ n) => Vector n -> (Vector k, Vector l)
splitV = view vec >>> VS.take k &&& VS.drop k >>> Vec *** Vec
  where k = natToInt @ k

concatVec :: forall n m. (KnownNat n, KnownNat m) => Vector n -> Vector m -> Vector (n + m)
concatVec (Vec v1) (Vec v2) = Vec (v1 VS.++ v2)

(<.>) :: Vector n -> Vector n -> Float
(<.>) (Vec a) (Vec b) = (LA.<.>) a b

(#>) :: Matrix m n -> Vector m -> Vector n
(#>) (Mat m) (Vec v) = Vec (m LA.#> v)

outer :: Vector m -> Vector n -> Matrix n m
outer (Vec a) (Vec b) = Mat (a `LA.outer` b)

appendV :: (KnownNat n, KnownNat m) => Vector n -> Vector m -> Vector (n+m)
appendV (Vec v1) (Vec v2) = Vec $ v1 VS.++ v2


instance forall n. KnownNat n => Num (Vector n) where
  (+) (Vec v1) (Vec v2) = Vec (v1 + v2)
  (*) (Vec v1) (Vec v2) = Vec (v1 * v2)
  negate (Vec v1)       = Vec (LD.cmap negate v1)
  fromInteger i         = fromListV [fromIntegral i]
  abs    (Vec v1)       = Vec (LD.cmap abs v1)
  signum (Vec v1)       = Vec (LD.cmap signum v1)

instance forall n. KnownNat n => Fractional (Vector n) where
  (/) (Vec v1) (Vec v2) = Vec (v1 / v2)
  fromRational          = fromValV . fromRational

instance forall n. KnownNat n => Floating (Vector n) where
  pi    = fromValV pi
  exp   = cmap exp
  log   = cmap log
  sin   = cmap sin
  cos   = cmap cos
  sinh  = cmap sinh
  cosh  = cmap cosh
  tanh  = cmap tanh
  asinh = cmap asinh
  acosh = cmap acosh
  atanh = cmap atanh

answer :: Vector n -> Int
answer (Vec v) = fst . VS.ifoldr (\i x b -> if x > snd b then (i,x) else b) (0,0) $ v
