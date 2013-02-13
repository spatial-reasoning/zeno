module Helpful.TimeIt
    ( timeIt
    , traceTime
    ) where

import Control.DeepSeq
import System.IO.Unsafe
import Data.Time.Clock.POSIX (getPOSIXTime)
        
timeIt :: (NFData a) => IO a -> IO (Double, a)
timeIt act = do
  start <- getTime
  result <- act
  end <- (return $!! result) >> getTime
  let delta = end - start
  return (delta, result)

traceTime :: (NFData a) => a -> a
traceTime act = unsafePerformIO $ do
  start <- getTime
  let result = act
  end <- (return $!! result) >> getTime
  let delta = end - start
  putStrLn $ show delta
  return result

getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime
