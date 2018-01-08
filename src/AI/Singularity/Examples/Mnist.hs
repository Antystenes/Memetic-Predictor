{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module AI.Singularity.Examples.Mnist where
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

import AI.Singularity.Network

getLabels :: String -> IO [Float]
getLabels = map (fromIntegral . fromEnum) . drop 8 . BS.unpack <$< BS.readFile

getImages2 :: String -> IO [[Vector 28]]
getImages2 = (map . map) fromListV . splitByLen 28 . splitByLen 28 . map ((/255). fromIntegral . fromEnum) . drop 16 . BS.unpack <$< BS.readFile

getSet3 :: String -> IO [((Vector 400, Vector 384), Vector 10)]
getSet3 = map (toVec >>> first splitV) <$< getSet

getSet2 :: String -> IO [([Vector 28],Vector 10)]
getSet2 nm = zip <$> getImages2 ("mnist/"++nm++"-images-idx3"++rest) <*> (map wynik <$> getLabels ("mnist/"++nm++"-labels-idx1"++rest))
  where rest          = "-ubyte"
        wynik liczba  = Vec . VS.map (\x -> if x == liczba then 1 else 0) . VS.fromList $ [0..9]

getImages :: String -> IO [VS.Vector Float]
getImages = map VS.fromList . splitByLen (28*28) . map ((/255). fromIntegral . fromEnum) . drop 16 . BS.unpack <$< BS.readFile

-- getImagesP :: FilePath -> Producer PB.ByteString IO a
getImagesC fn = let s = do
                      x <- CB.take 28
                      if BS.length x == 0
                        then return ()
                        else yield (map ((/255) . fromIntegral . fromEnum) . BS.unpack $ x) >> s
                    v = do
                      x <- CL.take 28
                      if null x
                        then return ()
                        else yield x >> v
                in CB.sourceFile fn .| (CB.drop 16 >> s) .| CL.map (fromListV :: [Float] -> Vector 28) .| v

getLinearC fn = let s = do
                      x <- BS.unpack <$> CB.take (28*28)
                      if null x
                        then return ()
                        else do
                              yield . map ((/255). fromIntegral . fromEnum) $ x
                              s
                in CB.sourceFile fn .| (CB.drop 16 >> s) .| CL.map (fromListV :: [Float] -> Vector 784)

fix f = let x = f x in x

-- setConduit :: String -> Source
setConduit nm = zipSources (getImagesC im) (getLabelsC lab)
  where im   = "mnist/"++nm++"-images-idx3"++rest
        lab  = "mnist/"++nm++"-labels-idx1"++rest
        rest = "-ubyte"

setConduit2 nm = zipSources (getLinearC im) (getLabelsC lab)
  where im   = "mnist/"++nm++"-images-idx3"++rest
        lab  = "mnist/"++nm++"-labels-idx1"++rest
        rest = "-ubyte"

getLabelsC fn =
  CB.sourceFile fn .|
  (CB.drop 8 >> fix (\f -> CB.head >>= \case { Nothing -> return (); Just x -> yield x >> f} )) .| CL.map (fromEnum >>> wynik)
    where wynik n = Vec . VS.map (\x -> if x == n then 1 else 0) . VS.fromList $ [0..9] :: Vector 10

  --map (fromIntegral . fromEnum) . drop 8 . BS.unpack

getSet :: String -> IO [(VS.Vector Float, Float)]
getSet nm = zip <$> getImages ("mnist/"++nm++"-images-idx3"++rest) <*> getLabels ("mnist/"++nm++"-labels-idx1"++rest)
  where rest = "-ubyte"

printSet :: (VS.Vector Float, Float) -> IO ()
printSet (v,a) = do
  print a
  mapM_ (\(i,val) -> (if i `mod` 28 == 0 then putStr (printVal val ++ "\n") else putStr (printVal val) )) . zip [1..] . VS.toList $ v

printVal :: Float -> String
printVal x = if x < 0.00001 then "  " else "* "

toVec :: (VS.Vector Float, Float) -> (Vector 784, Vector 10)
toVec (!obraz, !liczba) = (Vec obraz, Vec wynik)
    where wynik = VS.map (\x -> if x == liczba then 1 else 0) . VS.fromList $ [0..9]

printVec :: Vector 784 -> String
printVec (Vec !v) =  concatMap (\(i,val) -> (if i `mod` 28 == 0 then printVal val ++ "\n" else printVal val )) . zip [1..] . VS.toList $ v

testVec :: (VS.Vector Float, Float) -> (Vector 784, Int)
testVec (!obrac, !liczba) = (Vec obrac, round liczba)
