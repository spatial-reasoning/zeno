module Testsuite.Dipole80 where

-- standard modules
import qualified Data.Map as Map
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Dipole80


-- ----------------------------------------------------------------------------
-- -- Dipole 80 networks

forwardCircle :: Int -> Network [String] (Set.Set Dipole80)
forwardCircle n
    | n < 2      = eNetwork
    | otherwise  = Network
        { nDesc = show n ++ " nodes," ++
            " forming a loop of consecutively connected, collinear dipoles,\
            \ each in front of its predecessor."
        , nCalc = "Dipole-80"
        , nNumOfNodes = Just n
        , nCons = Map.fromList $ ([show $ n - 1, "0"], Set.singleton EFBS80) : map
            (\k -> ([show k, show $ k + 1], Set.singleton EFBS80)
            ) [0..n - 2] 
        }

circleWithTwoCollinearDipolesInside :: Int
                                    -> Network [String] (Set.Set Dipole80)
circleWithTwoCollinearDipolesInside n
    | n < 5      = eNetwork
    | otherwise  = Network
        { nDesc = show n ++ " nodes," ++
            " forming a circle with two collinear, dipoles inside, one in\
            \ front of the other, inside and outside of the circle at the\
            \ same time."
        , nCalc = "Dipole-80"
        , nNumOfNodes = Nothing
        , nCons = Map.fromList $
               [ ( [show 0              , show $ n - 2] , Set.singleton LLLR80 )
               , ( [show $ div (n - 2) 2, show $ n - 1] , Set.singleton LLRL80 )
               , ( [show $ n - 2        , show $ n - 1] , Set.singleton EFBS80 )
               , ( [show $ n - 3        , show 0      ] , Set.singleton ELLS80 )
               -- make inconsistent:
               , ( [show $ div ((n - 2) * 3) 4, show $ n - 1] , Set.singleton RRLLp80 )
               ]
            ++ map (\k -> ( [show k, show $ n - 3] , Set.singleton LLLL80 )) [1..n - 5]
            ++ foldl
               (\acc k ->
                   ( ( [show k, show $ k + 1] , Set.singleton ELLS80 ) : map
                       (\l -> ( [show k, show l] , Set.singleton LLLL80 )
                       ) [k + 2..n-4]
                   ) ++ acc
               ) [] [0..n-4]
        }

