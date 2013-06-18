module Backtracking where

type Choice a = [a]

choose :: [a] -> Choice a
choose xs = xs

-- Define a "zero" for our monad.  This
-- represents failure.
mzero :: Choice a
mzero = choose []

-- Either fail, or return something
-- useless and continue the computation.
guard :: Bool -> Choice ()
guard True  = return ()
guard False = mzero

