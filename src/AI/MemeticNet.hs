{-# LANGUAGE DataKinds #-}
module AI.MemeticNet where

import GHC.TypeLits
import System.IO (hPrint, stderr)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List   as CL
import Data.Conduit.Internal (zipSources)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.State as S
import qualified Data.ByteString.Char8 as CS
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map.Strict as HM
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Storable as VS
import Data.Conduit


import AI.Singularity.Data.Network
import AI.Singularity.Data.Vector
import AI.Singularity.Training
import AI.Singularity.Data.Matrix

-- MEMETIC

enumToOneHot :: (KnownNat n, Enum e) => e -> Vector n
enumToOneHot = convToOneHot fromEnum

convToOneHot :: (KnownNat n) => (e -> Int) -> e -> Vector n
convToOneHot c e = fromListV . map (\x -> if x == c e then 1 else 0) $ [0..]

memeticClass :: String -> IO (HM.Map BS.ByteString Int)
memeticClass fn = HM.fromList . flip zip [0..]. C.split '\n' <$> BS.readFile fn

memeticClassInv :: String -> IO (HM.Map Int BS.ByteString)
memeticClassInv fn = HM.fromList . zip [0..] . C.split '\n' <$> BS.readFile fn


evalUtterance :: (KnownNat m, KnownNat n) => Network [Vector n] (Vector m) ->  String -> [Float]
evalUtterance net ut =
  let Vec v = conductSignal (map enumToOneHot . BS.unpack . C.pack $ ut) net
  in  VS.toList v

memeticEvaluatorFactory netFilename = do
   l1 <- randomLayer sigmoid :: IO (Network (Vector 50) (Vector 17))
   g <- gru :: IO (Network [Vector 255] (Vector 50))
   net <- flip loadWeights (FFSeq g l1).read <$> readFile netFilename
   return $ evalUtterance net


getData f h = CB.sourceFile f
             .| CB.lines
             .| c
  where c =  do
          val <- await
          lift . lift $ hPrint stderr val
          case val of
            Just x -> case C.split '\t'. BS.fromStrict $ x of
              cl:seq:_ ->
                  let i = map enumToOneHot . C.unpack $ seq
                      o = convToOneHot id $ case HM.lookup cl h of
                                              Nothing -> 0
                                              Just val -> val
                  in yield (i ,o) >> c
              _ -> c
            Nothing -> return ()

checkData f h = runConduitRes
                 $  CB.sourceFile f
                .| CB.lines
                .| c
  where c =  do
          val <- await
          lift . lift $ hPrint stderr val
          case val of
            Just x -> case C.split '\t'. BS.fromStrict $ x of
              cl:seq:_ ->
                  let i = C.unpack $ seq
                      o = case HM.lookup cl h of
                                              Nothing -> 0
                                              Just val -> val
                  in (lift . lift $ hPrint stderr (i,o)) >> c

            Nothing -> return undefined
