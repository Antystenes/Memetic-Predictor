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
module AI.Singularity.Data.Matrix where
import GHC.TypeLits
import qualified Numeric.LinearAlgebra.Data as LD
import qualified Numeric.LinearAlgebra.Devel as LDev
import qualified Numeric.LinearAlgebra as LA
import Control.Lens
import Data.Proxy
import Control.Monad.ST
import AI.Singularity.Utils
import System.Random
import Control.Monad

data GeneralMatrix :: (* -> *) -> Nat -> Nat -> * where
  Mat :: mat Float -> GeneralMatrix mat m n

deriving instance Show (mat Float) => Show (GeneralMatrix mat m n)

type Matrix = GeneralMatrix LD.Matrix

type MutMatrix s = GeneralMatrix (LDev.STMatrix s)

matr :: Lens' (GeneralMatrix mat m n) (mat Float)
{-# INLINE matr #-}
matr f (Mat m) = Mat <$> f m

data GeneralInTrainingMat mat m n =
  GeneralInTrainingMat (GeneralMatrix mat m n) (GeneralMatrix mat m n) (GeneralMatrix mat m n)

type InTrainMat m n = GeneralInTrainingMat LD.Matrix m n

type STInTrainMat s = GeneralInTrainingMat (LDev.STMatrix s)

deriving instance Show (mat Float) => Show (GeneralInTrainingMat mat m n)

trMatr :: Lens' (GeneralInTrainingMat mat m n) (GeneralMatrix mat m n)
{-# INLINE trMatr #-}
trMatr f (GeneralInTrainingMat m g h) = (\x -> GeneralInTrainingMat x g h) <$> f m

trGrad :: Lens' (GeneralInTrainingMat mat m n) (GeneralMatrix mat m n)
{-# INLINE trGrad #-}
trGrad f (GeneralInTrainingMat m g h) = (\x -> GeneralInTrainingMat m x h) <$> f g

hGrad :: Lens' (GeneralInTrainingMat mat m n) (GeneralMatrix mat m n)
{-# INLINE hGrad #-}
hGrad f (GeneralInTrainingMat m g h) = GeneralInTrainingMat m g <$> f h


matSlice,gradSlice :: forall s m n. (KnownNat m, KnownNat n) => STInTrainMat s m n -> LDev.Slice s Float
matSlice  m = LDev.Slice (m^.trMatr.matr) 0 0 (fromIntegral . natVal $ Proxy @ n) (fromIntegral . natVal $ Proxy @ m)
gradSlice m = LDev.Slice (m^.trGrad.matr) 0 0 (fromIntegral . natVal $ Proxy @ n) (fromIntegral . natVal $ Proxy @ m)

thawM :: Matrix m n -> ST s (MutMatrix s m n)
thawM = Mat <$< LDev.thawMatrix . view matr


toSTInTrainM :: (KnownNat m, KnownNat n) => Matrix m n -> ST s (STInTrainMat s m n)
toSTInTrainM m = GeneralInTrainingMat <$> thawM m <*> thawM 0 <*> thawM 0

matHeight :: forall m n mat. KnownNat n => GeneralMatrix mat m n -> Int
matHeight _ = natToInt @ n

matWidth :: forall m n mat. KnownNat m => GeneralMatrix mat m n -> Int
matWidth _ = natToInt @ m


mat :: forall m n. (KnownNat m, KnownNat n) => [Float] -> Matrix m n
mat = Mat . (valm LD.>< valn) . take (valm * valn) . cycle
    where valm = matHeight ( undefined :: Matrix m n)
          valn = matWidth ( undefined :: Matrix m n)

randomMat :: forall m n. (KnownNat m, KnownNat n) => IO (Matrix m n)
randomMat = Mat . (valm LD.>< valn) <$> replicateM (valm * valn) (randomRIO (0.001, 0.1))
    where valm = matHeight ( undefined :: Matrix m n)
          valn = matWidth ( undefined :: Matrix m n)

zeroMat :: forall m n. (KnownNat m, KnownNat n) => Matrix m n
zeroMat = 0

ident :: forall n. (KnownNat n) => Matrix n n
ident = Mat $ LD.ident valn
  where valn = matHeight ( undefined :: Matrix n n)

transposeM :: forall m n. (KnownNat m, KnownNat n) => Matrix m n -> Matrix n m
transposeM = Mat . LA.tr . view matr

instance (KnownNat n, KnownNat m) => Num (Matrix m n) where
  (+) (Mat m1) (Mat m2) = Mat (m1 + m2)
  (*) (Mat m1) (Mat m2) = Mat (m1 * m2)
  negate (Mat m1)       = Mat $ LD.cmap negate m1
  fromInteger i         = mat [fromInteger i]
  abs (Mat m1)          = Mat $ LD.cmap abs m1
  signum (Mat m1)       = Mat $ LD.cmap signum m1

instance (KnownNat n, KnownNat m) => Fractional (Matrix m n) where
  (/) (Mat m1) (Mat m2) = Mat $ m1 / m2
  fromRational          = mat . (:[]) . fromRational

instance (KnownNat n, KnownNat m, n ~ m) => Monoid (Matrix n m) where
  mempty                    = Mat $ LA.ident $ fromInteger $ natVal (Proxy :: Proxy n)
  mappend (Mat m1) (Mat m2) = Mat $ (LA.<>) m1 m2

mmap :: (KnownNat n, KnownNat m) => (Float -> Float) -> Matrix m n -> Matrix m n
mmap f (Mat m) = Mat $ LD.cmap f m

checkNan :: forall m n. Matrix m n -> Matrix m n
checkNan (Mat x) = Mat (LD.cmap nancheck x)
