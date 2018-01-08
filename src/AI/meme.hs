class ZeroGrad f where
  zeroGrad :: f

-- gemv :: forall s a b m n. MutMatrix s m n -> MutVector s m -> MutVector s n -> ST s ()
-- gemv m v res = do

-- trainSTNet :: forall s a b. a -> b -> STNetwork s a b -> ST s a
-- trainSTNet inp outGr (FFLayer (GeneralInTrainingMat wght grad) f) = do

-- trainSample2 corpus trState =
--   if view epochs trState >= 1
--   then (foldl' applyTrainingExample trState >>> over epochs (subtract 1) >>> trainSample2 corpus) corpus
--   else fromTrain . applyGrad (view config trState) . view network . foldl' applyTrainingExample trState $ corpus

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- MEMETIC

enumToOneHot :: (KnownNat n, Enum e) => e -> Vector n
enumToOneHot = convToOneHot fromEnum

convToOneHot :: (KnownNat n) => (e -> Int) -> e -> Vector n
convToOneHot c e = fromListV . map (\x -> if x == c e then 1 else 0) $ [0..]

memeticClass :: String -> IO (HM.Map BS.ByteString Int)
memeticClass fn = HM.fromList . flip zip [0..]. C.split '\n' <$> BS.readFile fn

memeticClassInv :: String -> IO (HM.Map Int BS.ByteString)
memeticClassInv fn = HM.fromList . zip [0..] . C.split '\n' <$> BS.readFile fn


evalUtterance :: (KnownNat m, KnownNat n) => HM.Map Int BS.ByteString -> Network [Vector n] (Vector m) ->  BS.ByteString -> [(BS.ByteString, Float)]
evalUtterance m net ut =
  let Vec v = conductSignal (map enumToOneHot . BS.unpack $ ut) net
  in  map (\(i,p) -> (HM.findWithDefault (C.pack "Advice Yoda Gives") i m, p)) . zip [0..] . VS.toList $ v

interact m n = do
  l <- C.split '\n' <$> BS.getContents
  mapM_ (print . evalUtterance m n) l

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
