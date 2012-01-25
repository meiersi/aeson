{-# LANGUAGE BangPatterns, OverloadedStrings #-}

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Attoparsec
import Data.Time.Clock
import System.Environment (getArgs)
import System.IO
import qualified Data.ByteString as B
import Control.DeepSeq

main :: IO ()
main = do
  (cnt:args) <- getArgs
  let count = read cnt :: Int
  forM_ args $ \arg -> bracket (openFile arg ReadMode) hClose $ \h -> do
    putStrLn $ arg ++ ":"
    let refill = B.hGet h 16384
    result <- parseWith refill json =<< refill
    r <- case result of
           Done _ r -> return r
           _        -> fail $ "failed to read " ++ show arg
    start <- getCurrentTime
    let loop !n r
            | n >= count = return ()
            | otherwise = {-# SCC "loop" #-} do
          case result of
            Done _ r -> rnf (encode r) `seq` loop (n+1) r
            _        -> error $ "failed to read " ++ show arg
    loop 0 r
    delta <- flip diffUTCTime start `fmap` getCurrentTime
    let rate = fromIntegral count / realToFrac delta :: Double
    putStrLn $ "  " ++ show delta
    putStrLn $ "  " ++ show (round rate) ++ " per second"
