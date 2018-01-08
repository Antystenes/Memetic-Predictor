{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module AI.Singularity.Utils where

import Data.Proxy
import GHC.TypeLits
import Control.Arrow
import Debug.Trace

infixr 4 <$<
(<$<) :: (Functor f, Functor f1) => (a -> b) -> f1 (f a) -> f1 (f b)
(<$<) = fmap . fmap

natToInt :: forall n. KnownNat n => Int
natToInt = fromIntegral . natVal $ Proxy @ n

instance forall n k. (Num n, Num k) => Num (n,k) where
  (a,b) + (c,d) = (a+c,b+d)
  (a,b) * (c,d) = (a*c,b*d)
  negate        = negate *** negate
  fromInteger   = fromInteger &&& fromInteger
  abs           = abs *** abs
  signum        = signum *** signum

instance forall n k. (Fractional n, Fractional k) => Fractional (n,k) where
  (a,b) / (c,d) = (a/c,b/d)
  fromRational  = fromRational &&& fromRational

nancheck x = if isNaN x then trace "NAN" 0 else if isInfinite x then trace "Infinite" 1 else x
--  where invalid = isNaN \/ isInfinite

splitByLen :: Int -> [a] -> [[a]]
splitByLen n l =
  let ls   = drop n l
      rest = case ls of { [] -> []; _ -> splitByLen n ls}
  in take n l : rest
