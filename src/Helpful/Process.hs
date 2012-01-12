module Helpful.Process where

import Control.Concurrent
import Control.Exception
import System.Exit
import System.IO
import System.IO.Error
import System.IO.Unsafe
import System.Posix.Process
import System.Posix.Signals
import System.Process
import System.Process.Internals

runOnSafeProcessGen :: String -> [String] -> StdStream -> StdStream -> StdStream
                    -> ( (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO a )
                    -> a
runOnSafeProcessGen prog args streamIn streamOut streamErr fun = unsafePerformIO $ bracket
    ( do
        h <- createProcess (proc prog args) 
                 { std_in  = streamIn
                 , std_out = streamOut
                 , std_err = streamErr
                 , create_group = True }
        return h
    )
    (\(_, _, _, ph) -> terminateProcessGroup ph >> waitForProcess ph)
--    (\(_, _, _, ph) -> interruptProcessGroupOf ph >> waitForProcess ph)
    fun
{-# NOINLINE runOnSafeProcess #-}

runOnSafeProcess :: String -> [String]
                 -> ( (Handle, Handle, Handle) -> IO a )
                 -> a
runOnSafeProcess prog args fun =
    runOnSafeProcessGen prog args CreatePipe CreatePipe CreatePipe
        (\(Just inh, Just outh, Just errh, _) -> fun (inh, outh, errh) )

readSafeProcess :: String -> [String] -> String -> String
readSafeProcess prog args str =
    runOnSafeProcessGen prog args CreatePipe CreatePipe Inherit
      (\(Just inh, Just outh, _, ph) -> do
        hPutStr inh str
        hClose inh
        -- fork a thread to consume output
        output <- hGetContents outh
        outMVar <- newEmptyMVar
        forkIO $ evaluate (length output) >> putMVar outMVar ()
        -- wait on output
        takeMVar outMVar
        hClose outh
        return output
      )

terminateProcessGroup :: ProcessHandle -> IO ()
terminateProcessGroup ph = do
    let (ProcessHandle pmvar) = ph
    ph_ <- readMVar pmvar
    case ph_ of
        OpenHandle pid -> do  -- pid is a POSIX pid
--            gid <- createProcessGroup pid
            signalProcessGroup 15 pid
--            signalProcess 15 pid
        otherwise -> return ()

