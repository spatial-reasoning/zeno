module Interface.Yices where

-- standard modules

-- local modules
import Helpful.Process

readYices :: String -> IO String
readYices str = return $ readSafeProcess "yices" ["-smt"] str

