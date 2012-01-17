module Interface.Yices where

-- standard modules

-- local modules
import Helpful.Process

readYices :: String -> IO String
readYices str = return $ readSafeProcess "yices" ["-smt"] str
--readYices str = do
--    putStrLn str
--    return $ readSafeProcess "cvc3" ["-lang smtlib"] str

