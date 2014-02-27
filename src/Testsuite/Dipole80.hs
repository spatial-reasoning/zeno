module Testsuite.Dipole80 where

-- standard modules
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Dipole80


-- ----------------------------------------------------------------------------
-- -- Dipole 80 networks

forwardCircle :: Int -> Network [String] (GRel Dipole80)
forwardCircle n
    | n < 2      = eNetwork
    | otherwise  = Network
        { nDesc = show n ++ " nodes," ++
            " forming a loop of consecutively connected, collinear dipoles,\
            \ each in front of its predecessor."
        , nCalc = "Dipole-80"
        , nNumOfNodes = Just n
        , nCons = Map.fromList $
            ([show $ n - 1, "0"], GRel $ Set.singleton EFBS80) : map
                (\k -> ([show k, show $ k + 1], GRel $ Set.singleton EFBS80)
                ) [0..n - 2] 
        }

convexNGonWithTwoCollinearDipolesInside :: Int
                                        -> Network [String] (GRel Dipole80)
convexNGonWithTwoCollinearDipolesInside n
    | n < 5      = eNetwork
    | otherwise  = Network
        { nDesc = show n ++ " nodes," ++
            " forming a convex n-gon with two collinear, dipoles inside, one\
            \ in front of the other, inside and outside of the circle at the\
            \ same time."
        , nCalc = "Dipole-80"
        , nNumOfNodes = Nothing
        , nCons = Map.fromList $
               [ (["0"                 , show $ n - 2] , GRel $ Set.singleton LLLR80)
               , ([show $ div (n - 2) 2, show $ n - 1] , GRel $ Set.singleton LLRL80)
               , ([show $ n - 2        , show $ n - 1] , GRel $ Set.singleton EFBS80)
               , (["0"                 , show $ n - 3] , GRel $ Set.singleton LSEL80)
               -- make inconsistent:
               , ( [show $ div ((n - 2) * 3) 4, show $ n - 1]
                 , GRel $ Set.singleton RRLLp80 )
               ]
            ++ foldl
               (\acc k ->
                   ( ( [show k, show $ k + 1] , GRel $ Set.singleton ELLS80 ) : map
                         (\ l -> ( [show k, show l] , GRel $ Set.singleton LLLLP80 )
--                         ) [k + 2..n - 4]
                         ) [k + 2..min (n - 3) (k + div (n-3) 2)]
                     ++ let
                          oppositeSide = k + div (n-2) 2
                        in
                          if even n && oppositeSide < n - 2 then
                              [ ( [show k, show oppositeSide]
                                , GRel $ Set.singleton LLLLa80 )
                              ]
                          else
                              []
                     ++ map
                         (\ l -> ( [show k, show l] , GRel $ Set.singleton LLLLM80 )
                         ) [k + div n 2..n - 3]
                   ) ++ acc
               ) [] [0..n-4]
        }

properPartLoop :: Int -> Network [String] (GRel Dipole80)
properPartLoop n
    | n < 2      = eNetwork
    | otherwise  = Network
        { nDesc = show (3 * n) ++ " nodes," ++ " forming a proper part loop of " ++ show n ++ " triangles."
        , nCalc = "Dipole-80"
        , nNumOfNodes = Just $ 3 * n
        , nCons = Map.fromList $ foldl
            (\ acc i -> (++ acc) $
                [ ( [show $ 3*i    , show $ 3*i + 1], GRel $ Set.singleton ELLS80 )
                , ( [show $ 3*i + 1, show $ 3*i + 2], GRel $ Set.singleton ELLS80 )
                , ( [show $ 3*i    , show $ 3*i + 2], GRel $ Set.singleton LSEL80 )
                , ( [show $ 3*i    , show $ (3*i + 3) `mod` (3 * n)], GRel startAndEndPointLeft )
                , ( [show $ 3*i    , show $ (3*i + 4) `mod` (3 * n)], GRel startAndEndPointLeft )
                , ( [show $ 3*i    , show $ (3*i + 5) `mod` (3 * n)], GRel startAndEndPointLeft )
                , ( [show $ 3*i + 1, show $ (3*i + 3) `mod` (3 * n)], GRel startAndEndPointLeft )
                , ( [show $ 3*i + 1, show $ (3*i + 4) `mod` (3 * n)], GRel startAndEndPointLeft )
                , ( [show $ 3*i + 1, show $ (3*i + 5) `mod` (3 * n)], GRel startAndEndPointLeft )
                , ( [show $ 3*i + 2, show $ (3*i + 3) `mod` (3 * n)], GRel startAndEndPointLeft )
                , ( [show $ 3*i + 2, show $ (3*i + 4) `mod` (3 * n)], GRel startAndEndPointLeft )
                , ( [show $ 3*i + 2, show $ (3*i + 5) `mod` (3 * n)], GRel startAndEndPointLeft )
                ]
            )
            []
            [0..n-1]
        }



indianNGon :: Int -> Network [String] (GRel Dipole80)
indianNGon n
    | n < 3      = eNetwork
    | otherwise  = Network
        { nDesc = show n ++ " nodes," ++
            " forming a leftward convex n-gon and a dipole with its startpoint\
            \ inside the n-gon and its endpoint outside. This network is made\
            \ inconsistent through the constraint that the dipole does not\
            \ intersect the n-gon."
        , nCalc = "Dipole-80"
        , nNumOfNodes = Just $ n + 1
        , nCons = Map.fromList $ foldl
            (\ acc i ->
                [ ( [show i, show $ i + 1], GRel $ Set.singleton ELLS80 )
                , ( [show i, show $ n + 1]
                  , GRel $ Set.intersection disconnected startPointLeft
                  )
                ] ++ map
                    (\ j -> ([show i, show j], GRel disconnected)
                    ) [i+2..n]
                ++ acc
            )
            [ ( ["1", show n], GRel $ Set.singleton LSEL80 )
            , ( [show n, show $ n + 1]
              , GRel $ Set.intersection disconnected startPointLeft
              )
            , ( ["1", show $ n + 1]
              , GRel $ foldl1 (\ acc relSet -> Set.intersection relSet acc
                              ) [disconnected, startPointLeft, endPointNotLeft]
              )
            ]
            [1..n-1]
        }

disconnected :: Set.Set Dipole80
disconnected = Set.fromList $ filter
    (\ rel -> (null $ intersect "ISE" $ show rel)
           && (not $ elem rel [LRRL80, RLLR80])
    ) cBaserelationsList

startAndEndPointLeft :: Set.Set Dipole80
startAndEndPointLeft = Set.fromList $ filter
    (\ rel -> (take 2 $ show rel) == "LL"
    ) cBaserelationsList

startPointLeft :: Set.Set Dipole80
startPointLeft = Set.fromList $ filter
    (\ rel -> (head $ show rel) == 'L'
    ) cBaserelationsList

endPointRight :: Set.Set Dipole80
endPointRight = Set.fromList $ filter
    (\ rel -> (show rel)!!1 == 'R'
    ) cBaserelationsList

endPointNotLeft :: Set.Set Dipole80
endPointNotLeft = Set.fromList $ filter
    (\ rel -> (show rel)!!1 /= 'L'
    ) cBaserelationsList

