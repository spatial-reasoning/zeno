module Helpful.Directory where

import Control.Exception
import System.Directory
import System.Random

createTempDir :: String -> IO FilePath
createTempDir dirName = do
    randGen <- newStdGen
    sysTmpDir <- getTemporaryDirectory
    let tmpDirPrefix = sysTmpDir ++ "/" ++ dirName ++ "_"
    tmpDir <- nonExistingDirName tmpDirPrefix
    createDirectory tmpDir
    return tmpDir
  where
    nonExistingDirName prefix = do
        randint <- randomRIO (1, 9999999 :: Int)
        let path = prefix ++ show randint
        fileExists <- doesFileExist path
        dirExists <- doesDirectoryExist path
        if (fileExists || dirExists) then nonExistingDirName prefix
        else return path

-- |create a temporary directory, run the action, remove the temporary directory
-- the first argument is a template for the temporary directory name
-- the directory will be created as a subdirectory of the directory returned by getTemporaryDirectory
-- the temporary directory will be automatically removed afterwards.
-- your working directory is not altered
withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir fp f = bracket
    (createTempDir fp)
    removeDirectoryRecursive -- this function follows symlinks !!!
    f

