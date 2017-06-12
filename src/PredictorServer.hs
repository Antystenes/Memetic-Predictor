{-# LANGUAGE OverloadedStrings#-}
module PredictorServer
    ( someFunc
    ) where

import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp (run)
import qualified Data.HashMap.Strict as HM
import qualified Text.JSON    as J
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Data.Text (Text)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified System.Process as P
import           Control.Arrow
import           Control.Monad
import           System.IO
import           Network.URI

type Memes = HM.HashMap Int Text

getMemes :: IO Memes
getMemes = foldr insertToMap HM.empty . T.lines <$> TIO.readFile "memes"
  where insertToMap line map =
          let [nr,meme] = T.splitOn "\t" line
          in HM.insert (read . T.unpack $ nr) meme map

initPy :: Handle -> IO ()
initPy hndl = hGetLine hndl >>= \x -> (x == "Using Theano backend.") `unless` (putStrLn x >> initPy hndl)

someFunc :: IO ()
someFunc = do
  ( hin, _, herr, pr) <- P.runInteractiveCommand "python2 predict.py"
  memes                  <- getMemes
  initPy herr
  let
    printHdl         = hShow >=> putStrLn

    readOut          = (++) <$> hGetLine herr <*> hGetLine herr

    jSonify question = J.encode . J.makeObj . (:[(J.toJSKey ("question"::String), J.showJSON question)]). (,) (J.toJSKey ("results" :: String)) . J.makeObj . map ((flip (HM.lookupDefault " ") memes >>> T.unpack >>> J.toJSKey) *** J.showJSON) . zip [1..] .  (read :: String -> [Float]) . map (\x -> if x == ' ' then ',' else x)

    simpleApp :: Application
    simpleApp a respond = do
      let query = unEscapeString . tail . BS.unpack . rawQueryString $ a
      hPutStrLn hin query
      hFlush hin
      putStrLn $ "Processing: " ++ query
      response <- jSonify query <$> readOut
      respond $ responseLBS
        status200
        [("Content-Type","application/json")]
        (BL.pack response)

  hPutStrLn hin "Initiate"
  hFlush hin
  readOut
  putStrLn "startingServer"
  run 8080 simpleApp