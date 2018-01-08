{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module AI.Singularity.Training where
import GHC.TypeLits
import qualified Numeric.LinearAlgebra.Data as LD
import qualified Numeric.LinearAlgebra.Devel as LDev
import qualified Numeric.LinearAlgebra as LA
import qualified Control.Monad.State as S
import Control.Lens
import Data.Proxy
import Control.Monad.ST
import System.Random
import Control.Monad
import Control.Arrow
import qualified Data.Vector.Storable as VS
import Data.Conduit
import System.IO (hPrint, stderr)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List   as CL
import Data.Conduit.Internal (zipSources)
import Numeric.AD
import Control.Monad.Trans.Class (lift)

import AI.Singularity.Utils
import AI.Singularity.Data.Matrix
import AI.Singularity.Data.Vector
import AI.Singularity.Data.Network

data TrainConfig = TrainConfig { _learningRate :: !Float,
                                 _lrUpdate     :: Float -> Float,
                                 _batchsize    :: !Int,
                                 _momentumRate :: !Float }
makeLenses ''TrainConfig

newtype Loss = Loss { getLoss :: forall a. Floating a => a -> a -> a}

lossGrad :: Loss -> (forall a. Floating a => a -> a -> a)
lossGrad (Loss f) = f'
  where fList :: forall a. Floating a => [a] -> a
        fList [y,y'] = f y y'
        f' y y'      = head . tail . grad fList $ [y,y']

data TrainState i o = TrainState { _config  :: !TrainConfig,
                                   _network :: !(TrainedNetwork i o),
                                   _iter    :: !Int,
                                   _loss    :: Loss,
                                   _epochs  :: !Int,
                                   _evalSet :: [(i,o)]}

makeLenses ''TrainState


addGrad :: (KnownNat m, KnownNat n) => TrainConfig -> InTrainMat m n -> Identity (InTrainMat m n)
addGrad tr mp = Identity $ GeneralInTrainingMat newWeights newGradient hGr
  where momentumRatio = view momentumRate tr
        lr            = view learningRate tr
        newWeights    = checkNan $ view trMatr mp + (mmap (*lr) $ view trGrad mp) / mmap sqrt (hGr + 0.00000001)
        newGradient   = checkNan . over matr (LD.cmap (*momentumRatio)) $ view trGrad mp
        hGr           = 0.9 * view hGrad mp + 0.1 * (view trGrad mp ^ 2)


applyGrad :: TrainConfig -> TrainedNetwork inp out -> TrainedNetwork inp out
applyGrad tr = runIdentity . changeMatType (addGrad tr)

calcGrads :: forall n m. (KnownNat n, KnownNat m) => Vector n -> Vector m -> TrainedNetwork (Vector n) (Vector m) -> (Matrix (n+1) m, Vector n)
calcGrads !inp !outGrad (FFLayer !mpair !f) = (gradMat, inpGrad)
  where
    m       = view trMatr mpair
    gradMat = dy `outer` consV 1 inp
    dy      = outGrad * diffM
    diffM   = cmap (diff f) . (#>) m . consV 1 $ inp
    inpGrad = tailV $ transposeM m #> dy

trainWithErr :: forall a b. TrainConfig -> a -> b -> TrainedNetwork a b -> (TrainedNetwork a b, a)
trainWithErr tc !inp !outGrad n@(FFLayer !mpair !f) = (FFLayer  m' f, inpGrad)
  where lr = view learningRate tc
        m' = over trGrad (subtract gradMat) mpair
        (gradMat,inpGrad) = calcGrads inp outGrad n
trainWithErr tc !inp !outGrad (FFSeq !n1 !n2) = (FFSeq n1' n2', inpGrad)
  where (n1', inpGrad)  = trainWithErr tc inp inpGrad' n1
        inp'            = conductSignalInTrain inp n1
        (n2', inpGrad') = trainWithErr tc inp' outGrad n2
trainWithErr tc !inp !outGrad (SplitNet n) = (SplitNet *** splitV) unSpl
  where unSpl = trainWithErr tc (uncurry appendV inp) outGrad n
trainWithErr tc !inp !outGrad (FromBinFunc f) = (FromBinFunc f, grads)
  where grads = (\(x1,x2) -> ((!!0) &&& (!!1)). map (*outGrad) . fgrad $ [x1,x2]) inp
        fwrap :: forall a. Fractional a => [a] -> a
        fwrap [x1,x2] = f x1 x2
        fgrad = grad fwrap
trainWithErr tc !inp !outGrad (Split n1 n2) = (Split n1' n2', inGr)
  where (n1', inGr1) = trainWithErr tc inp (fst outGrad) n1
        (n2', inGr2) = trainWithErr tc inp (snd outGrad) n2
        inGr         = inGr1 + inGr2
trainWithErr tc !inp !outGrad (FromFunc f) = (FromFunc f, inGr)
  where inGr = diff f inp * outGrad
trainWithErr tc !inp !outGrad (FFDiv !n1 !n2) = (FFDiv n1' n2', (inGr1, inGr2))
  where (n1', inGr1) = trainWithErr tc (fst inp) (fst outGrad) n1
        (n2', inGr2) = trainWithErr tc (snd inp) (snd outGrad) n2
trainWithErr tc !inp !outGrad (Recurse n)   = if null inp then (Recurse n, []) else (Recurse n', [fst inGrad])
   where inputs      = scanl (\acc sym -> (sym, conductSignalInTrain acc n)) (head inp,0) (tail inp)
         (n',inGrad) = foldr (\input (net, outG) -> trainWithErr tc input (snd outG) net) (n,(undefined, outGrad)) inputs
trainWithErr tc !inp !outGrad (First n)     = (First n', inGrad)
  where (n', in1Grad) = trainWithErr tc (fst inp) outGrad n
        inGrad        = (in1Grad, 0)
trainWithErr tc !inp !outGrad (Second n)     = (Second n', inGrad)
  where (n', in1Grad) = trainWithErr tc (snd inp) outGrad n
        inGrad        = (0, in1Grad)

trainSample :: forall a b. TrainConfig -> [(a,b)] -> (b -> b -> b) -> Int -> Network a b -> Network a b
trainSample tc !corpus gLoss !epochs = fromTrain . (trainHelp tc 1 . concat $ replicate epochs corpus) . toTrain
  where
    trainHelp :: TrainConfig -> Int -> [(a, b)] -> TrainedNetwork a b -> TrainedNetwork a b
    trainHelp tc i ((!x,!y):corp) !net = next . fst $ trainWithErr tc x l net
      where l    = gLoss y (conductSignalInTrain x net)
            next = if i < b
                   then trainHelp tc (i+1) corp
                   else trainHelp nTc 1 corp . applyGrad tc
            nTc  = tc -- over learningRate (view lrUpdate tc) tc
            b    = view batchsize tc
    trainHelp _ _ [] !net = net


eps :: forall n. Floating n => n
eps = 0.00000001

defaultConfig = TrainConfig 0.05 id 1 0

endOfBatch :: TrainState i o -> Bool
endOfBatch = view iter &&& view (config.batchsize) >>> uncurry (>=)

startState conf net = defaultState conf net logloss
  where logloss y y' = negate $ y * log (y' + eps) + (1 - y) * log (1 - y' + eps)

defaultState :: TrainConfig -> Network i o -> (forall a. Floating a => a -> a -> a) -> Int -> [(i,o)] -> TrainState i o
defaultState conf net f ep = TrainState conf (toTrain net) 1 (Loss f) ep

-- applyTrainingExample :: (Floating o, Show o) => TrainState i o -> (i,o) -> IO (TrainState i o)
applyTrainingExample tr (i,o) = endBatch . over iter (+1) . over network (fst . trainWithErr trConf i outLoss) $ tr
  where
    outLoss  = lossGrad (view loss tr) o (conductSignalInTrain i (view network tr))
    trConf   = view config tr
    endBatch = if endOfBatch tr
               then \x -> do
                           let up = view (config.lrUpdate) x
                               nX = over (config.learningRate) up . set iter 1 . over network (applyGrad trConf) $ x
                               s  = view evalSet tr
                               ac = sum . map (uncurry. flip . getLoss . view loss $ tr) . map (first $ flip conductSignalInTrain (view network nX)) $ s
                           unless (null s) . lift $ do
                             hPrint stderr "LOSS:"
                             hPrint stderr (ac/fromIntegral (length s))
                           return nX
               else return

trainC cond conf net ep = trainStateC cond . startState conf net ep

trainStateC cond trState =
  if view epochs trState >= 1
  then
    runConduitRes (cond .| CL.foldM applyTrainingExample trState) >>= (over epochs (subtract 1) >>> trainStateC cond)
  else
    fromTrain . applyGrad (view config trState) . view network <$> runConduitRes (cond .| CL.foldM applyTrainingExample trState)

trainExample :: forall a b. Floating b => TrainConfig -> [(a, b)] -> Int -> Network a b -> Network a b
trainExample conf x = trainSample conf x logloss'
  where
    logloss [y, y'] = y * log (y' + eps) + (1 - y) * log (1 - y' + eps)
    logloss' y y'   = head . tail . grad logloss $ [y, y']
