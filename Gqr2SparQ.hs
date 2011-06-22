{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main (main) where

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import Control.Monad (when)
import Export
import Parsing

version = "                                        ."
authors = "André Scholz (andre.scholz@uni-bremen.de)"

data MyOptions = MyOptions
   { infilename :: FilePath
   , outfilename :: FilePath
--   } deriving (Data, Typeable, Show, Eq)
   } deriving (Data, Typeable, Show, Eq)

-- Customize your options, including help messages, shortened names, etc.
myProgOpts :: MyOptions
myProgOpts = MyOptions
    { infilename = def &= explicit &= name "i" &= name "in" &= help "read file from FN trinaetrn iaetrnia edtrinae dtrina edtriane dtriuane dtiurane dtriuane utirane iutrane "
    , outfilename = def &= explicit &= name "o" &= name "out" &= help "write file to FN"
    }

getOpts :: IO MyOptions
getOpts = cmdArgs $ myProgOpts
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary _PROGRAM_INFO
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME = "Gqr2SparQ"
_PROGRAM_VERSION = "                                ."
_PROGRAM_INFO = _PROGRAM_NAME ++ " -- " ++ _COPYRIGHT ++ "\nversion:" ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "converts constraint networks from GQR to SparQ format and vice versa"
_COPYRIGHT = "(Rite2Copy) André Scholz (andre.scholz@uni-bremen.de)"

main :: IO ()
main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) getOpts
    optionHandler opts

-- Before directly calling your main program, you should warn your user about
-- incorrect arguments, if any.
optionHandler :: MyOptions -> IO ()
optionHandler opts@MyOptions{..}  = do
    -- Take the opportunity here to weed out ugly, malformed, or invalid
    -- arguments.
    when (null outfilename) $ putStrLn "no argument for --out given!" >> exitWith (ExitFailure 1)
    when (null infilename) $ putStrLn "no argument for --in given!" >> exitWith (ExitFailure 1)
    -- When you're done, pass the (corrected, or not) options to your actual program.
    exec opts

exec :: MyOptions -> IO ()
exec opts@MyOptions{..} = do
    network <- loadGqrNetworkFile infilename
    exportToSparQ network outfilename

