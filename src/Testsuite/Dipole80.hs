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
        , nCons = Map.fromList $
            ([show $ n - 1, "0"], Set.singleton EFBS80) : map
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
               [ (["0"                 , show $ n - 2] , Set.singleton LLLR80)
               , ([show $ div (n - 2) 2, show $ n - 1] , Set.singleton LLRL80)
               , ([show $ n - 2        , show $ n - 1] , Set.singleton EFBS80)
               , ([show $ n - 3        , "0"         ] , Set.singleton ELLS80)
               -- make inconsistent:
               , ( [show $ div ((n - 2) * 3) 4, show $ n - 1]
                 , Set.singleton RRLLp80 )
               ]
            ++ foldl
               (\acc k ->
                   ( ( [show k, show $ k + 1] , Set.singleton ELLS80 ) : map
                         (\ l -> ( [show k, show l] , Set.singleton LLLLP80 )
--                         ) [k + 2..n - 4]
                         ) [k + 2..min (n - 3) (k + div (n-3) 2)]
                     ++ let
                          oppositeSide = k + div (n-2) 2
                        in
                          if even n && oppositeSide < n - 2 then
                              [ ( [show k, show oppositeSide]
                                , Set.singleton LLLLa80 )
                              ]
                          else
                              []
                     ++ map
                         (\ l -> ( [show k, show l] , Set.singleton LLLLM80 )
                         ) [k + div n 2..n - 3]
                   ) ++ acc
               ) [] [0..n-4]
        }

