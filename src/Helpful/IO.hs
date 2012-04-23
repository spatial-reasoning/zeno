module Helpful.IO where

import System.IO

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd a = hReady hnd >>= f
  where
    f True = a >>= return . Just
    f _    = return Nothing

keyPressed :: Char -> IO Bool
keyPressed a = do
    x <- hGetContentsWithoutClosing stdin
    return $ maybe False (elem a) x

hGetContentsWithoutClosing :: Handle -> IO (Maybe String)
hGetContentsWithoutClosing h = ifReadyDo h $ do
    x <- getChar
    y <- hGetContentsWithoutClosing h
    return $ maybe [x] (x:) y

