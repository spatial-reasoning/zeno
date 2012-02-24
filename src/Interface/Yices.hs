module Interface.Yices where

-- standard modules
import Debug.Trace

-- local modules
import Helpful.Process

readYices :: String -> String
readYices str = unsafeReadProcess "yices" ["-smt"] str
--readYices str = readSafeProcess "opensmt" ["-smt"] str
--readYices str = do
--    putStrLn str
--    return $ readSafeProcess "cvc3" ["-lang smtlib"] str

