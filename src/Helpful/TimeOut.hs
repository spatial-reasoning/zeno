module Helpful.TimeOut where

import Control.Concurrent
 
compete :: [IO a] -> IO a
compete actions = do
    mvar <- newEmptyMVar
    tids <- mapM (\action -> forkIO $ action >>= putMVar mvar) actions
    result <- takeMVar mvar
    mapM_ killThread tids
    return result

timeout :: Int -> IO a -> IO (Maybe a)
timeout usec action =
    compete [fmap Just action, threadDelay usec >> return Nothing]

timeoutP :: Int -> a -> IO (Maybe a)
timeoutP usec action = timeout usec $ return $! action
