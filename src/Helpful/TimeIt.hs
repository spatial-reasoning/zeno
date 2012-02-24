module Helpful.TimeIt
    ( timeIt
    ) where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.DeepSeq
        
timeIt :: (NFData a) => IO a -> IO (Double, a)
timeIt act = do
  start <- getTime
  result <- act
  end <- (return $!! result) >> getTime
  let delta = end - start
  return (delta, result)

getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime
