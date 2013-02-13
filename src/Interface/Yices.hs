module Interface.Yices where

-- standard modules
import Data.List
import Debug.Trace

-- local modules
import Helpful.Process

readYices :: String -> (String, String)
readYices str = unsafeReadProcess "yices" ["-smt"] str
--readYices str = readSafeProcess "opensmt" ["-smt"] str
--readYices str = do
--    putStrLn str
--    return $ readSafeProcess "cvc3" ["-lang smtlib"] str

yicesSat :: String -> Bool
yicesSat str = case (sat, unsat) of
    (True, False) -> True
    (False, True) -> False
    (_, _)        -> error $ "Help! Yices answered: " ++ str ++ "\n" ++ err
  where
    (out, err) = readYices str
    sat   = or $ map (\x -> "sat"   `isPrefixOf` x) $ lines out
    unsat = or $ map (\x -> "unsat" `isPrefixOf` x) $ lines out

