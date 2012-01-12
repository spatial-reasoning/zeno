module Helpful.Timeout
    ( timeout
    , timeoutP
    ) where

--import Control.Concurrent
import System.Timeout

-- timeout pure functions:
timeoutP :: Int -> a -> IO (Maybe a)
timeoutP usec action = timeout usec $ return $! action


-- here is a homemade version found on haskellwiki:
--compete :: [IO a] -> IO a
--compete actions = do
--    mvar <- newEmptyMVar
--    tids <- mapM (\action -> forkIO $ action >>= putMVar mvar) actions
--    result <- takeMVar mvar
--    mapM_ killThread tids
--    return result
--
--timeout :: Int -> IO a -> IO (Maybe a)
--timeout usec action =
--    compete [fmap Just action, threadDelay usec >> return Nothing]

