module Helpful.Process where

import Control.Concurrent
import Control.Exception
import System.IO
import System.IO.Unsafe
import System.Posix.Signals
import System.Process
import System.Process.Internals

safeCreateProcess :: String -> [String] -> StdStream -> StdStream -> StdStream
                  -> ( ( Maybe Handle
                       , Maybe Handle
                       , Maybe Handle
                       , ProcessHandle
                       ) -> IO a )
                  -> IO a
safeCreateProcess prog args streamIn streamOut streamErr fun = bracket
    ( do
        h <- createProcess (proc prog args) 
                 { std_in  = CreatePipe
                 , std_out = CreatePipe
                 , std_err = CreatePipe
                 , create_group = True }
        return h
    )
--    (\(_, _, _, ph) -> interruptProcessGroupOf ph >> waitForProcess ph)
    (\(_, _, _, ph) -> terminateProcessGroup ph >> waitForProcess ph)
    fun
{-# NOINLINE safeCreateProcess #-}

safeReadProcess :: String -> [String] -> String -> IO (String, String)
safeReadProcess prog args str =
    safeCreateProcess prog args CreatePipe CreatePipe Inherit
      (\(Just inh, Just outh, Just errh, ph) -> do
        hPutStr inh str
        hClose inh
        -- fork a thread to consume output
        output <- hGetContents outh
        outMVar <- newEmptyMVar
        forkIO $ evaluate (length output) >> putMVar outMVar ()
        -- fork a thread to consume output
        outputErr <- hGetContents errh
        errMVar <- newEmptyMVar
        forkIO $ evaluate (length outputErr) >> putMVar errMVar ()
        -- wait on output
        takeMVar outMVar
        hClose outh
        takeMVar errMVar
        hClose errh
        return (output, outputErr)
--        ex <- waitForProcess ph
--        case ex of
--            ExitSuccess -> return output
--            ExitFailure r ->
--                fail ("spawned process " ++ prog ++ " exit: " ++ show r)
      )

unsafeReadProcess :: String -> [String] -> String -> (String, String)
unsafeReadProcess prog args str =
    unsafePerformIO $ safeReadProcess prog args str
{-# NOINLINE unsafeReadProcess #-}

terminateProcessGroup :: ProcessHandle -> IO ()
terminateProcessGroup ph = do
    let (ProcessHandle pmvar) = ph
    ph_ <- readMVar pmvar
    case ph_ of
        OpenHandle pid -> do  -- pid is a POSIX pid
            signalProcessGroup 15 pid
        otherwise -> return ()

