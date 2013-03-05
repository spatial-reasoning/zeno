module DecisionProcedure.Otop.TriangleConsistency where

import qualified Data.Foldable as Fold
import qualified Data.Key as Key
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Ratio
import qualified Data.Set as Set
import System.IO.Unsafe

-- local modules
import Basics
import Helpful.General
import Helpful.TimeIt
import Interface.Yices
import SpatioTemporalStructure.OrientedPoint


-- maximal granularity for which this code should work.
maxgran = 4

halfcircle :: Integer
halfcircle = foldr1 lcm [1..maxgran]

circle = halfcircle * 2

angle g s = quot (halfcircle * fromIntegral (quot s 2)) (fromIntegral g)
minAngle g s = if even s then angle g s else angle g (s - 1)
maxAngle g s = if even s then angle g s else angle g (s + 1)

data Term = Fo  Formula
          | Con Integer
          | Var [String]
          | Neg Term
          | Add Term Term
          | Sub Term Term
          deriving (Eq, Ord)

instance Show Term where
    show term = case term of
        Fo  f     -> show f
        Con c     -> show c
        Var xs    -> "v_" ++ intercalate "_" xs
        Neg t     -> "-" ++ show t
        Add t1 t2 -> "(" ++ show t1 ++ " + " ++ show t2 ++ ")"
        Sub t1 t2 -> "(" ++ show t1 ++ " - " ++ show t2 ++ ")"

data Formula = Te  Term
             | Eq  Term Term
             | Le  Term Term
             | Leq Term Term
             | And Formula Formula
             | Or  Formula Formula
             | Iff Formula Formula
             | Ite Formula Formula Formula
             | Tru
             | Fals
             deriving (Eq, Ord)

instance Show Formula where
    show e = case e of
        Te  t        -> show t
        Eq  t1 t2    -> "(" ++ show t1 ++ " = "  ++ show t2 ++ ")"
        Le  t1 t2    -> "(" ++ show t1 ++ " < "  ++ show t2 ++ ")"
        Leq t1 t2    -> "(" ++ show t1 ++ " <= " ++ show t2 ++ ")"
        And f1 f2    -> "(" ++ show f1 ++ " && " ++ show f2 ++ ")"
        Or  f1 f2    -> "(" ++ show f1 ++ " || " ++ show f2 ++ ")"
        Iff f1 f2    -> "(" ++ show f1 ++ " <=> " ++ show f2 ++ ")"
        Ite f1 f2 f3 -> "(if " ++ show f1 ++ " then " ++ show f2
                                          ++ " else " ++ show f3 ++ ")"
        Tru          -> "True"
        Fals         -> "False"

class ShowSMT a where
    showSMT :: a -> String

-- maybe we need to add newlines here.
instance ShowSMT Term where
    showSMT term = case term of
        Fo  f     -> showSMT f
        Var xs    -> "v_" ++ intercalate "_" xs
        Con c     -> show c
        Add t1 t2 -> "(+ " ++ showSMT t1 ++ " " ++ showSMT t2 ++ ")"
        Sub t1 t2 -> "(- " ++ showSMT t1 ++ " " ++ showSMT t2 ++ ")"
        Neg t     -> "(- 0 " ++ showSMT t ++ ")"

instance ShowSMT Formula where
    showSMT e = case e of
        Te  t        -> showSMT t
        Eq  t1 t2    -> "(= "    ++ showSMT t1 ++ " "  ++ showSMT t2 ++ ")"
        Le  t1 t2    -> "(< "    ++ showSMT t1 ++ " "  ++ showSMT t2 ++ ")"
        Leq t1 t2    -> "(<= "   ++ showSMT t1 ++ " "  ++ showSMT t2 ++ ")"
        And f1 f2    -> "(and\n" ++ showSMT f1 ++ "\n" ++ showSMT f2 ++ ")"
        Or  f1 f2    -> "(or\n"  ++ showSMT f1 ++ "\n" ++ showSMT f2 ++ ")"
        Iff f1 f2    -> "(iff\n" ++ showSMT f1 ++ "\n" ++ showSMT f2 ++ ")"
        Ite f1 f2 f3 -> "(ite\n" ++ showSMT f1 ++ "\n" ++ showSMT f2 ++ "\n"
                                                       ++ showSMT f3 ++ ")"
        Tru          -> "true"
        Fals         -> "false"


getVarsTe :: Term -> Set.Set Term
getVarsTe (Fo  f)     = getVarsFo f
getVarsTe (Con _)     = Set.empty
getVarsTe (Var v)     = Set.singleton (Var v)
getVarsTe (Neg t)     = getVarsTe t
getVarsTe (Add t1 t2) = Set.union (getVarsTe t1) (getVarsTe t2)
getVarsTe (Sub t1 t2) = Set.union (getVarsTe t1) (getVarsTe t2)

getVarsFo :: Formula -> Set.Set Term
getVarsFo (Te  t)        = getVarsTe t
getVarsFo (Eq  t1 t2)    = Set.union (getVarsTe t1) (getVarsTe t2)
getVarsFo (Le  t1 t2)    = Set.union (getVarsTe t1) (getVarsTe t2)
getVarsFo (Leq t1 t2)    = Set.union (getVarsTe t1) (getVarsTe t2)
getVarsFo (And f1 f2)    = Set.union (getVarsFo f1) (getVarsFo f2)
getVarsFo (Or  f1 f2)    = Set.union (getVarsFo f1) (getVarsFo f2)
getVarsFo (Iff f1 f2)    = Set.union (getVarsFo f1) (getVarsFo f2)
getVarsFo (Ite f1 f2 f3) = Set.unions [getVarsFo f1,getVarsFo f2,getVarsFo f3]
getVarsFo Tru            = Set.empty
getVarsFo Fals           = Set.empty


-- This function assumes, that the Otop relations are similar to the
-- corresponding opra relations.
-- "Same" relations are mapped to a negative granularity
-- and then indicate the direction of the second opoint with respect to
-- the first.
--
-- signum g = -1 denotes a 'SAME' relation,
-- signum g =  1 denotes a non-'SAME' relation,
-- signum g =  0 and signum s = -1 denotes the sole knowledge
-- that the pair is in a 'SAME' relation (without any knowledge
-- about directions)
-- signum g =  0 and signum s =  1 denotes the sole knowledge
-- that the pair is in a non-'SAME' relation (without any
-- knowledge about directions)
-- signum g =  0 and signum s =  0 denotes no knowledge at all.
--
-- Note: Add converses of same relations before giving the network to this
-- function in order to improve the reasoning.
-- improve: catch empty network.
-- improve: find a better way to preprocess the network.


-- | Translate a network of SECTOR relations into a QF_LRA equation.
-- The parameter 'useWitnesses' selects whether to use witnesses to decide
-- whether a pair lies in a 'SAME' relation or not.
translateToTrianglesOrig :: Bool -> Bool -> Network [String] (ARel Otop) -> Maybe Formula
translateToTrianglesOrig useWitness useWitnesses net@Network{nCons = cons} = do
    let allNodes = nodesIn $ nCons net
    let pairs    = kCombinations 2 $ Set.toAscList allNodes
    -- Fold over all pairs and generate:
    --   1. the map mapping pairs to either True or False depending on whether
    --      they are in a 'SAME' relation or not. Pairs for which we have no
    --      information are left out of the map.
    --   2. the map mapping nodes to the set of nodes of which we know whether
    --      the node lies in a 'SAME' relation to them or not. Nodes for which
    --      we have no information are left out of the map respectively the
    --      set.
    -- abbr.: pairsWithSameness_and_relatedNodes
    ps_rn <- Fold.foldlM
        (\ (acc, acc2) pair@[node, node2] ->
          let
            -- Can we find a third node for which node and node2 lie in
            -- different relations?
            -- fixme: rename this function as it also can detect sameness.
            [foundWitnessForUnsameness, foundWitnessForSameness] =
                if useWitness then
                    map (not . Set.null) $ Set.foldr
                      (\ node3 acc@[accUnsame, accSame] ->
                        let
                          n3n  = Map.lookup [node3, node ] cons
                          n3n2 = Map.lookup [node3, node2] cons
                          ARel (Otop gran_n3n  sec_n3n) =
                              fromJust n3n
                          ARel (Otop gran_n3n2 sec_n3n2) =
                              fromJust n3n2
                        in
                          if node /= node3 && node2 /= node3 &&
                             isJust n3n && isJust n3n2 && n3n /= n3n2 then
                              if 
                                  signum gran_n3n /= (-1) || signum gran_n3n2 /= (-1)
                              then
                                  [Set.insert node3 accUnsame, accSame]
                              else
                                  [accUnsame, Set.insert node3 accSame]
                          else
                              acc
                      ) [Set.empty, Set.empty] allNodes
                else
                    [False, False]
            foundWitnessesForSameness =
                if useWitnesses then
                    not $ null $ filter
                      (\ [node3, node4] ->
                        let
                          n3n  = Map.lookup [node3, node ] cons
                          n3n2 = Map.lookup [node3, node2] cons
                          n3n4 = Map.lookup [node3, node4] cons
                          n4n  = Map.lookup [node4, node ] cons
                          n4n2 = Map.lookup [node4, node2] cons
                          n4n3 = Map.lookup [node4, node3] cons
                          ARel (Otop gran_n3n  sec_n3n)  =
                              fromJust n3n
                          ARel (Otop gran_n3n2 sec_n3n2) =
                              fromJust n3n2
                          ARel (Otop gran_n3n4 sec_n3n4) =
                              fromJust n3n4
                          ARel (Otop gran_n4n  sec_n4n)  =
                              fromJust n4n
                          ARel (Otop gran_n4n2 sec_n4n2) =
                              fromJust n4n2
                          ARel (Otop gran_n4n3 sec_n4n3) =
                              fromJust n4n3
                        in
                          -- improve: This could be improved, e.g. by
                          -- adding gran_n3n == 0 && gran_n3n2 == 0 etc.
                          isJust n3n && isJust n3n2 && isJust n4n && isJust n4n2
                          && gran_n3n > 0 && gran_n3n2 > 0
                          && gran_n4n > 0 && gran_n4n2 > 0 
                          && even sec_n3n && even sec_n4n
                          -- n and n2 lie on the same line
                          && (sec_n3n * gran_n3n2 == sec_n3n2 * gran_n3n)
                          && (sec_n4n * gran_n4n2 == sec_n4n2 * gran_n4n)
                          -- n3 and n4 not in line with n and n2
                          && (  (isJust n3n4
                                && (sec_n3n * gran_n3n4 /= sec_n3n4 * gran_n3n)
                                && (sec_n3n * gran_n3n4 /=
                                    mod (sec_n3n4 + 2 * gran_n3n4) (4 * gran_n3n4)
                                    * gran_n3n))
                             || (isJust n4n3
                                && (sec_n4n * gran_n4n3 /= sec_n4n3 * gran_n4n)
                                && (sec_n4n * gran_n4n3 /=
                                    mod (sec_n4n3 + 2 * gran_n4n3) (4 * gran_n4n3)
                                    * gran_n4n))
                             )
                      ) $ filter (/= pair) pairs
                else
                  False
            sameHelper g s g2 s2 = case sort [ (signum g , signum s )
                                             , (signum g2, signum s2)] of
                -- signum g = -1 denotes a 'SAME' relation,
                -- signum g =  1 denotes a non-'SAME' relation,
                -- signum g =  0 and signum s = -1 denotes the sole knowledge
                -- that the pair is in a 'SAME' relation (without any knowledge
                -- about directions)
                -- signum g =  0 and signum s =  1 denotes the sole knowledge
                -- that the pair is in a non-'SAME' relation (without any
                -- knowledge about directions)
                -- signum g =  0 and signum s =  0 denotes no knowledge at all.
                [(-1, _), (-1, _)] -> newAccsSame
                [( 1, _), ( 1, _)] -> newAccsUnsame
                [(-1, _), ( 0, 1)] -> Nothing
                [(-1, _), ( 0, _)] -> newAccsSame
                [( 0,-1), ( 1, _)] -> Nothing
                [( 0, _), ( 1, _)] -> newAccsUnsame
                [( 0,-1), ( 0, 1)] -> Nothing
                [( 0,-1), ( 0, _)] -> newAccsSame
                [( 0, _), ( 0, 1)] -> newAccsUnsame
                [(-1, _), ( 1, _)] -> Nothing
                [( 0, 0), ( 0, 0)] -> newAccsUnknown
            noInfoHelper = case ( foundWitnessForUnsameness
                                , foundWitnessForSameness ||
                                  foundWitnessesForSameness   ) of
                (True , False) -> newAccsUnsame
                (False, True ) -> newAccsSame
                (False, False) -> newAccsUnknown
                (True , True ) -> Nothing
            newAccsSame  = Just (Map.insert pair True  acc, newAcc2)
            newAccsUnsame = Just (Map.insert pair False acc, newAcc2)
            newAccsUnknown = Just (acc, acc2)
            newAcc2 = Map.insertWith Set.union node (Set.singleton node2) acc2
          in
            case ( Map.lookup [node, node2] cons
                 , Map.lookup [node2, node] cons ) of
                (Just (ARel (Otop 0 0)), Just (ARel (Otop 0  0 ))) -> noInfoHelper
                (Just (ARel (Otop g s)), Just (ARel (Otop g2 s2))) -> sameHelper g s g2 s2
                (Just (ARel (Otop g s)), Nothing) -> sameHelper g s g s
                (Nothing, Just (ARel (Otop g s))) -> sameHelper g s g s
                (Nothing, Nothing) -> noInfoHelper
        ) (Map.empty, Map.empty) pairs
    let (pairsWithSameness, relatedNodes) = ps_rn
    translateToTrianglesOrig' cons pairsWithSameness relatedNodes


translateToTrianglesOrig' :: Map.Map [String] (ARel Otop)
                          -> Map.Map [String] Bool
                          -> Map.Map  String  (Set.Set String)
                          -> Maybe Formula
translateToTrianglesOrig' cons pairsWithSameness relatedNodes =
    equNewPairs
  where
    equNewPairs = Key.foldrWithKeyM
        (\ pair@[node, node2] same acc -> do
          let
            revPair = [node2, node]
            varPair = Var pair
            varRevPair = Var revPair
            pairCons = Map.lookup pair cons
            revPairCons = Map.lookup revPair cons
            noInfoSame =
                And (Leq (Con 0) varPair) (Le varPair (Con circle))
            noInfoNotSame = And
                (And (Leq (Con 0) varPair   )
                     (Le varPair    (Con circle)))
                (And (Leq (Con 0) varRevPair)
                     (Le varRevPair (Con circle)))
            fstSame g s = equKnownVar varPair (-g) s
            sndSame g s =
                if even s then
                    Eq varPair (Con $ circle - angle (-g) s)
                else
                    And (Le (Con $ circle - angle (-g) (s + 1))
                            varPair)
                        (Le varPair
                            (Con $ circle - angle (-g) (s - 1)))
            fstNotSame g s = notSame varPair varRevPair g s
            sndNotSame g s = notSame varRevPair varPair g s
            notSame knownVar unknownVar g s = And
                (And (Leq (Con 0) unknownVar)
                     (Le unknownVar (Con circle))) $
                equKnownVar knownVar g s
            equKnownVar var g s =
                if even s then
                    Eq var (Con $ angle g s)
                else
                    And (Le (Con $ angle g (s - 1)) var)
                        (Le var (Con $ angle g (s + 1)))
            equPair = case ( pairCons
                           , revPairCons
                           , same
                           ) of
                (Just (ARel (Otop 0 _)), Just (ARel (Otop 0  _ )), True ) -> noInfoSame
                (Nothing        , Nothing          , True ) -> noInfoSame
                (Just (ARel (Otop 0 _)), Just (ARel (Otop 0  _ )), False) -> noInfoNotSame
                (Nothing        , Nothing          , False) -> noInfoNotSame
                (Just (ARel (Otop g s)), Just (ARel (Otop 0  _ )), True ) -> fstSame    g s
                (Just (ARel (Otop g s)), Nothing          , True ) -> fstSame    g s
                (Just (ARel (Otop g s)), Just (ARel (Otop 0  _ )), False) -> fstNotSame g s
                (Just (ARel (Otop g s)), Nothing          , False) -> fstNotSame g s
                (Just (ARel (Otop 0 _)), Just (ARel (Otop g  s )), True ) -> sndSame    g s
                (Nothing        , Just (ARel (Otop g  s )), True ) -> sndSame    g s
                (Just (ARel (Otop 0 _)), Just (ARel (Otop g  s )), False) -> sndNotSame g s
                (Nothing        , Just (ARel (Otop g  s )), False) -> sndNotSame g s
                (Just (ARel (Otop g s)), Just (ARel (Otop g2 s2)), True ) -> 
                    if even s then
                        Eq varPair $ Con $ angle (-g) s
                    else if even s2 then
                        Eq varPair (Con $ circle - angle (-g2) s2)
                    else
                        And (Le (Con $ max (angle (-g) (s - 1))
                                           (circle - angle (-g2) (s2 + 1)))
                                varPair)
                            (Le varPair
                                (Con $ min (angle (-g) (s + 1))
                                           (circle - angle (-g2) (s2 - 1))))
                (Just (ARel (Otop g s)), Just (ARel (Otop g2 s2)), False) ->
                    And (equKnownVar varPair g s)
                        (equKnownVar varRevPair g2 s2)

          equTriples <- Fold.foldrM
              (\ node3 acc2 -> do
                let
                  pair2 = [node , node3]
                  pair3 = [node2, node3]
                  revPair2 = [node3, node ]
                  revPair3 = [node3, node2]
                  pair2Cons = Map.lookup pair2 cons
                  revPair2Cons = Map.lookup revPair2 cons
                  pair3Cons = Map.lookup pair3 cons
                  revPair3Cons = Map.lookup revPair3 cons
                  thirdNode a = head $ [node, node2, node3]\\a
                  startToThird [a,b] = [a, thirdNode [a,b]]
                  endToThird   [a,b] = [b, thirdNode [a,b]]
                  triple [a,b] = [a, thirdNode [a,b], b]
                  equUnsameTriple pairA pairB pairC =
                      And (
                      And (
                      And
                      (equUnsameTriple' pairA)
                      (equUnsameTriple' pairB))
                      (equUnsameTriple' pairC))
                      (Eq (Add (Var $ triple pairA) $ Add (Var $ triple pairB)
                                                          (Var $ triple pairC))
                          (Con halfcircle))
                  equUnsameTriple' pair'@[pair'1,pair'2] =
                      Eq (Var $ triple pair')
                         (Fo $ Ite (Leq (Var [thirdNode pair', pair'1])
                                        (Var [thirdNode pair', pair'2]))
                                   (Te $ Sub (Var [thirdNode pair', pair'2])
                                             (Var [thirdNode pair', pair'1]))
                                   (Te $ Add (Var [thirdNode pair', pair'2])
                                             (Sub (Con circle)
                                                  (Var [thirdNode pair', pair'1]))))

                helper <- case sort [ (same, pair)
                                    , (pairsWithSameness Map.! pair2, pair2)
                                    , (pairsWithSameness Map.! pair3, pair3)
                                    ] of
                  -- improve: use sectors to check whether we need "Ite".
                  -- This replaces the "ite"s and might dramatically improve
                  -- this method.
                      [(False, a), (False, b), (False, c)] -> Just $
                          Ite (Or (And (Leq (Con 0)
                                            (Sub (Var b) (Var a)))
                                       (Leq (Sub (Var b) (Var a))
                                            (Con halfcircle)))
                                  (And (Leq (Con 0)
                                            (Add (Var b)
                                                 (Sub (Con circle) (Var a))))
                                       (Leq (Add (Var b)
                                                 (Sub (Con circle) (Var a)))
                                            (Con halfcircle))))
                              (equUnsameTriple c (reverse b) a)
                              (equUnsameTriple (reverse c) b (reverse a))
                      [(False, _), (False, _), (True , pairC)] -> Just $
                         -- improve: use sectors to check whether we need "Ite"
                          (Eq (Var $ startToThird pairC)
                              (Fo $ Ite (Le (Add (Var pairC) (Var $ endToThird pairC))
                                            (Con circle))
                                        (Te $ Add (Var pairC) (Var $ endToThird pairC))
                                        (Te $ Sub (Add (Var pairC)
                                                       (Var $ endToThird pairC))
                                                  (Con circle))))
                      [(True , pairA), (True , pairB), (True , pairC)] -> Just $
                         -- improve: use sectors to check whether we need "Ite"
                          (Eq (Var pairB)
                              (Fo $ Ite (Le (Add (Var pairA) (Var pairC))
                                            (Con circle))
                                        (Te $ Add (Var pairA) (Var pairC))
                                        (Te $ Sub (Add (Var pairA) (Var pairC))
                                                  (Con circle)) ))
                      [(False, _), (True, _), (True, _)] -> Nothing
                Just $ And acc2 helper
              ) acc $ Set.intersection
                          (maybe Set.empty id $ Map.lookup node  relatedNodes)
                          (maybe Set.empty id $ Map.lookup node2 relatedNodes)
          Just $ And equPair equTriples
        ) Tru pairsWithSameness



translateToTriangles :: Bool -> Bool -> Network [String] (ARel Otop) -> Maybe Formula
translateToTriangles useWitness useWitnesses net@Network{nCons = cons} = do
    let allNodes = Set.toAscList $ nodesIn $ nCons net
    let pairs    = kCombinations 2 allNodes
    -- Fold over all pairs and generate:
    --   1. the map mapping pairs to either True or False depending on whether
    --      they are in a 'SAME' relation or not. Pairs for which we have no
    --      information are left out of the map.
    --   2. the map mapping nodes to the set of nodes of which we know whether
    --      the node lies in a 'SAME' relation to them or not. Nodes for which
    --      we have no information are left out of the map respectively the
    --      set.
    -- abbr.: pairsWithSameness_and_relatedNodes
    ps_rn <- Fold.foldlM
        (\ (acc, acc2) pair@[node, node2] ->
          let
            -- Can we find a third node for which node and node2 lie in
            -- different relations - or both in the same relation?
            [foundWitnessForUnsameness, foundWitnessForSameness] =
                if useWitness then
                    foldr
                      (\ node3 acc@[accUnsame, accSame] ->
                        let
                          n3n  = Map.lookup [node3, node ] cons
                          n3n2 = Map.lookup [node3, node2] cons
                          ARel (Otop g1 s1) = fromJust n3n
                          ARel (Otop g2 s2) = fromJust n3n2
                          newAccSame = [accUnsame, True]
                          newAccUnsame = [True, accSame]
                          anglesDontMatch = case (odd s1, odd s2) of
                              (False, False) -> angle g1 s1 /= angle g2 s2
                              (False, True ) ->
                                  angle g1 s1 <= minAngle g2 s2 ||
                                  angle g1 s1 >= maxAngle g2 s2
                              (True, False) ->
                                  angle g2 s2 <= minAngle g1 s1 ||
                                  angle g2 s2 >= maxAngle g1 s1
                              (True , True ) -> 
                                  maxAngle g1 s1 <= minAngle g2 s2 ||
                                  minAngle g1 s1 >= maxAngle g2 s2
                        in
                          if node /= node3 && node2 /= node3 &&
                             isJust n3n && isJust n3n2
                          then case sort [ (signum g1, signum s1)
                                         , (signum g2, signum s2) ] of
                              [(-1, _), (-1, _)] -> newAccSame
                              [(-1, _), ( 0,-1)] -> newAccSame
                              [(-1, _), ( 0, 1)] -> newAccUnsame
                              [(-1, _), ( 1, _)] -> newAccUnsame
                              [( 0,-1), ( 0,-1)] -> newAccSame
                              [( 0,-1), ( 0, 1)] -> newAccUnsame
                              [( 0,-1), ( 1, _)] -> newAccUnsame
                              [( 1, _), ( 1, _)] ->
                                  if anglesDontMatch then newAccUnsame else acc
                              otherwise          -> acc
                          else
                             acc
                      ) [False, False] allNodes
                else
                    [False, False]
            foundWitnessesForSameness =
                if useWitnesses then
                    not $ null $ filter
                      (\ [node3, node4] ->
                        let
                          n3n  = Map.lookup [node3, node ] cons
                          n3n2 = Map.lookup [node3, node2] cons
                          n3n4 = Map.lookup [node3, node4] cons
                          n4n  = Map.lookup [node4, node ] cons
                          n4n2 = Map.lookup [node4, node2] cons
                          n4n3 = Map.lookup [node4, node3] cons
                          ARel (Otop gran_n3n  sec_n3n ) = fromJust n3n
                          ARel (Otop gran_n3n2 sec_n3n2) = fromJust n3n2
                          ARel (Otop gran_n3n4 sec_n3n4) = fromJust n3n4
                          ARel (Otop gran_n4n  sec_n4n ) = fromJust n4n
                          ARel (Otop gran_n4n2 sec_n4n2) = fromJust n4n2
                          ARel (Otop gran_n4n3 sec_n4n3) = fromJust n4n3
                        in
                          -- improve: This could be improved, e.g. by
                          -- adding gran_n3n == 0 && gran_n3n2 == 0 etc.
                          isJust n3n && isJust n3n2 && isJust n4n && isJust n4n2
                          && gran_n3n > 0 && gran_n3n2 > 0
                          && gran_n4n > 0 && gran_n4n2 > 0 
                          && even sec_n3n && even sec_n4n
                          -- n and n2 lie on the same line
                          && (sec_n3n * gran_n3n2 == sec_n3n2 * gran_n3n)
                          && (sec_n4n * gran_n4n2 == sec_n4n2 * gran_n4n)
                          -- n3 and n4 not in line with n and n2
                          && (  (isJust n3n4
                                && (sec_n3n * gran_n3n4 /= sec_n3n4 * gran_n3n)
                                && (sec_n3n * gran_n3n4 /=
                                    mod (sec_n3n4 + 2 * gran_n3n4) (4 * gran_n3n4)
                                    * gran_n3n))
                             || (isJust n4n3
                                && (sec_n4n * gran_n4n3 /= sec_n4n3 * gran_n4n)
                                && (sec_n4n * gran_n4n3 /=
                                    mod (sec_n4n3 + 2 * gran_n4n3) (4 * gran_n4n3)
                                    * gran_n4n))
                             )
                      ) $ filter (/= pair) pairs
                else
                  False
            firstTestAnglesForSames g s g2 s2 =
                if (even s && even s2 && angle (-g) s /= circle - angle (-g2) s2) ||
                   (even s && odd s2 &&
                       (angle (-g) s <= circle - angle (-g2) (s2 + 1)) ||
                       (angle (-g) s >= circle - angle (-g2) (s2 - 1))    ) ||
                   (odd s && even s2 &&
                       (angle (-g2) s2 <= circle - angle (-g) (s + 1)) ||
                       (angle (-g2) s2 >= circle - angle (-g) (s - 1))    ) ||
                   (angle (-g) (s - 1) <= circle - angle (-g2) (s2 + 1)) ||
                   (angle (-g) (s + 1) >= circle - angle (-g2) (s2 - 1))
                then
                    Nothing
                else
                    newAccsSame
            newAccsSame  =
                if foundWitnessForUnsameness then
                    Nothing
                else
                    newAccsSame'
            newAccsSame'  = Just (Map.insert pair True  acc, newAcc2)
            newAccsUnsame =
                if foundWitnessForSameness || foundWitnessesForSameness then
                    Nothing
                else
                    newAccsUnsame'
            newAccsUnsame' = Just (Map.insert pair False acc, newAcc2)
            newAccsUnknown = Just (acc, acc2)
            newAcc2 = Map.insertWith Set.union node (Set.singleton node2) acc2
            noInfoHelper = case ( foundWitnessForUnsameness
                                , foundWitnessForSameness ||
                                  foundWitnessesForSameness   ) of
                (True , False) -> newAccsUnsame'
                (False, True ) -> newAccsSame'
                (False, False) -> newAccsUnknown
                (True , True ) -> Nothing
            sameHelper g s g2 s2 = case sort [ (signum g , signum s )
                                             , (signum g2, signum s2)] of
                [(-1, 1), (-1, 1)] -> firstTestAnglesForSames g s g2 s2
                [(-1, _), (-1, _)] -> newAccsSame
                [( 1, _), ( 1, _)] -> newAccsUnsame
                [(-1, _), ( 0, 1)] -> Nothing
                [(-1, _), ( 0, _)] -> newAccsSame
                [( 0,-1), ( 1, _)] -> Nothing
                [( 0, _), ( 1, _)] -> newAccsUnsame
                [( 0,-1), ( 0, 1)] -> Nothing
                [( 0,-1), ( 0, _)] -> newAccsSame
                [( 0, _), ( 0, 1)] -> newAccsUnsame
                [(-1, _), ( 1, _)] -> Nothing
                [( 0, 0), ( 0, 0)] -> noInfoHelper
          in
            case ( Map.lookup [node, node2] cons
                 , Map.lookup [node2, node] cons ) of
                (Just (ARel (Otop 0 0)), Just (ARel (Otop 0  0 ))) -> noInfoHelper
                (Just (ARel (Otop g s)), Just (ARel (Otop g2 s2))) -> sameHelper g s g2 s2
                (Just (ARel (Otop g s)), Nothing)  -> sameHelper g s 0 0
                (Nothing, Just (ARel (Otop g s)))  -> sameHelper 0 0 g s
                (Nothing, Nothing)                 -> noInfoHelper
        ) (Map.empty, Map.empty) pairs
    let (pairsWithSameness, relatedNodes) = ps_rn
    translateToTriangles' cons pairsWithSameness relatedNodes


translateToTriangles' :: Map.Map [String] (ARel Otop)
                      -> Map.Map [String] Bool
                      -> Map.Map  String  (Set.Set String)
                      -> Maybe Formula
translateToTriangles' cons pairsWithSameness relatedNodes =
    equNewPairs
  where
    -- try to make a foldrWithKeyM1
    equNewPairs = Key.foldrWithKeyM
        (\ pair@[node, node2] same acc -> do
          let
            revPair = [node2, node]
            varPair = Var pair
            varRevPair = Var revPair
            pairCons = Map.lookup pair cons
            revPairCons = Map.lookup revPair cons
            noInfoSame = Just $
                And (Leq (Con 0) varPair) (Le varPair (Con circle))
            noInfoNotSame = Just $ And
                (And (Leq (Con 0) varPair   )
                     (Le varPair    (Con circle)))
                (And (Leq (Con 0) varRevPair)
                     (Le varRevPair (Con circle)))
            fstSame g s = Just $ equKnownVar varPair (-g) s
            sndSame g s = Just $
                if even s then
                    Eq varPair (Con $ circle - angle (-g) s)
                else
                    And (Le (Con $ circle - angle (-g) (s + 1))
                            varPair)
                        (Le varPair
                            (Con $ circle - angle (-g) (s - 1)))
            fstNotSame g s = Just $ notSame varPair varRevPair g s
            sndNotSame g s = Just $ notSame varRevPair varPair g s
            notSame knownVar unknownVar g s = And
                (And (Leq (Con 0) unknownVar)
                     (Le unknownVar (Con circle))) $
                equKnownVar knownVar g s
            equKnownVar var g s =
                if even s then
                    Eq var (Con $ angle g s)
                else
                    And (Le (Con $ angle g (s - 1)) var)
                        (Le var (Con $ angle g (s + 1)))
          equPair <- case ( pairCons
                          , revPairCons
                          , same
                          ) of
                (Just (ARel (Otop 0 _)), Just (ARel (Otop 0  _ )), True ) -> noInfoSame
                (Nothing        , Nothing          , True ) -> noInfoSame
                (Just (ARel (Otop 0 _)), Just (ARel (Otop 0  _ )), False) -> noInfoNotSame
                (Nothing        , Nothing          , False) -> noInfoNotSame
                (Just (ARel (Otop g s)), Just (ARel (Otop 0  _ )), True ) -> fstSame    g s
                (Just (ARel (Otop g s)), Nothing          , True ) -> fstSame    g s
                (Just (ARel (Otop g s)), Just (ARel (Otop 0  _ )), False) -> fstNotSame g s
                (Just (ARel (Otop g s)), Nothing          , False) -> fstNotSame g s
                (Just (ARel (Otop 0 _)), Just (ARel (Otop g  s )), True ) -> sndSame    g s
                (Nothing        , Just (ARel (Otop g  s )), True ) -> sndSame    g s
                (Just (ARel (Otop 0 _)), Just (ARel (Otop g  s )), False) -> sndNotSame g s
                (Nothing        , Just (ARel (Otop g  s )), False) -> sndNotSame g s
                (Just (ARel (Otop g s)), Just (ARel (Otop g2 s2)), True ) ->
                    -- improve by testing if angles match:
                    case (even s, even s2) of
                        (True , True ) ->
                            if angle (-g) s /= circle - angle (-g2) s2 then
                                Nothing
                            else
                                Just $ Eq varPair $ Con $ angle (-g) s
                        (True , False) ->
                            if angle (-g) s <= circle - angle (-g2) (s2 + 1) ||
                               angle (-g) s >= circle - angle (-g2) (s2 - 1)
                            then
                                Nothing
                            else
                                Just $ Eq varPair $ Con $ angle (-g) s
                        (False, True ) ->
                            if angle (-g2) s2 <= circle - angle (-g) (s + 1) ||
                               angle (-g2) s2 >= circle - angle (-g) (s - 1)
                            then
                                Nothing
                            else
                                Just $ Eq varPair
                                          (Con $ circle - angle (-g2) s2)
                        (False, False) -> Just $
                            And (Le (Con $ max (angle (-g) (s - 1))
                                               (circle - angle (-g2) (s2 + 1)))
                                    varPair)
                                (Le varPair
                                    (Con $ min (angle (-g) (s + 1))
                                               (circle - angle (-g2) (s2-1))))
                (Just (ARel (Otop g s)), Just (ARel (Otop g2 s2)), False) -> Just $
                    And (equKnownVar varPair g s)
                        (equKnownVar varRevPair g2 s2)

          -- improve: try to make a foldrM1
          equTriples <- Fold.foldrM
              (\ node3 acc2 -> do
                let
                  pair2 = [node , node3]
                  pair3 = [node2, node3]
                  revPair2 = [node3, node ]
                  revPair3 = [node3, node2]
                  thirdNode a = head $ [node, node2, node3]\\a
                  startToThird [a,b] = [a, thirdNode [a,b]]
                  endToThird   [a,b] = [b, thirdNode [a,b]]
                  triple [a,b] = [a, thirdNode [a,b], b]
                  equUnsameTriple tripleA tripleB tripleC =
                      And (
                      And (
                      And
                      (equUnsameTriple' tripleA)
                      (equUnsameTriple' tripleB))
                      (equUnsameTriple' tripleC))
                      (Eq (Add (Var tripleA) $ Add (Var tripleB) (Var tripleC))
                          (Con halfcircle))
                  equUnsameTriple' triple'@[triple'1, triple'2, triple'3] =
                    let
                      anglepointToFirst  = [triple'2, triple'1]
                      anglepointToSecond = [triple'2, triple'3]
                    in
                      Eq (Var triple')
                         (Fo $ Ite (Leq (Var anglepointToFirst)
                                        (Var anglepointToSecond))
                                   (Te $ Sub (Var anglepointToSecond)
                                             (Var anglepointToFirst))
                                   (Te $ Add (Var anglepointToSecond)
                                             (Sub (Con circle)
                                                  (Var anglepointToFirst))))

                helper <- case sort [ (same, pair)
                                    , (pairsWithSameness Map.! pair2, pair2)
                                    , (pairsWithSameness Map.! pair3, pair3)
                                    ] of
                      [(False, [a,b]), (False, [_,c]), (False, _)] ->
                       Just $
                        let
                          cornersWithFullInfo = filter
                              (\ (x, y) -> isJust x && isJust y)
                              [ (Map.lookup [a,b] cons, Map.lookup [a,c] cons)
                              , (Map.lookup [b,c] cons, Map.lookup [b,a] cons)
                              , (Map.lookup [c,a] cons, Map.lookup [c,b] cons)]
                          generalCase =
                              Ite (Or (And (Leq (Con 0)
                                                (Sub (Var [a,c]) (Var [a,b])))
                                           (Leq (Sub (Var [a,c]) (Var [a,b]))
                                                (Con halfcircle)))
                                      (And (Leq (Con 0)
                                                (Add (Var [a,c])
                                                     (Sub (Con circle)
                                                          (Var [a,b]))))
                                           (Leq (Add (Var [a,c])
                                                     (Sub (Con circle)
                                                          (Var [a,b])))
                                                (Con halfcircle))))
                                  (equUnsameTriple [b,a,c] [c,b,a] [a,c,b])
                                  (equUnsameTriple [c,a,b] [a,b,c] [b,c,a])
                        in
                          if False && any
                             (\ (Just (ARel (Otop g s)), Just (ARel (Otop g2 s2))) ->
                               (0 <= minAngle g2 s2 - maxAngle g s && maxAngle g2 s2 - minAngle g s <= halfcircle)
                               || maxAngle g2 s2 + (circle - minAngle g s) <= halfcircle
                             ) cornersWithFullInfo
                          then
                              equUnsameTriple [b,a,c] [c,b,a] [a,c,b]
                          else if False && any
                             (\ (Just (ARel (Otop g2 s2)), Just (ARel (Otop g s))) ->
                               (0 <= minAngle g2 s2 - maxAngle g s && maxAngle g2 s2 - minAngle g s <= halfcircle)
                               || maxAngle g2 s2 + (circle - minAngle g s) <= halfcircle
                             ) cornersWithFullInfo
                          then
                              equUnsameTriple [c,a,b] [a,b,c] [b,c,a]
                          else
                              generalCase
                      [(False, _), (False, _), (True , xY)] -> Just $
                         -- improve: use sectors to check whether we need "Ite"
                          (Eq (Var $ startToThird xY)
                              (Fo $ Ite (Le (Add (Var xY) (Var $ endToThird xY))
                                            (Con circle))
                                        (Te $ Add (Var xY) (Var $ endToThird xY))
                                        (Te $ Sub (Add (Var xY)
                                                       (Var $ endToThird xY))
                                                  (Con circle))))
                      [(True , aB), (True , aC), (True , bC)] -> Just $
                         -- improve: use sectors to check whether we need "Ite"
                          (Eq (Var aC)
                              (Fo $ Ite (Le (Add (Var aB) (Var bC))
                                            (Con circle))
                                        (Te $ Add (Var aB) (Var bC))
                                        (Te $ Sub (Add (Var aB) (Var bC))
                                                  (Con circle)) ))
                      [(False, _), (True, _), (True, _)] -> Nothing
                Just $ And acc2 helper
              ) acc $ Set.intersection
                          (maybe Set.empty id $ Map.lookup node  relatedNodes)
                          (maybe Set.empty id $ Map.lookup node2 relatedNodes)
          Just $ And equPair equTriples
        ) Tru pairsWithSameness



preamble :: String -> Formula -> String
preamble desc f = "(benchmark " ++ desc' ++ "\n\n" ++
--    ":logic QF_AUFLIA\n" ++
    ":logic QF_LRA\n" ++ ":extrafuns (\n" ++ Set.foldl
    (\ acc v -> acc ++ "  (" ++ showSMT v ++ " Real)\n"
    ) "" (getVarsFo f) ++ ")\n"
  where
    desc' = if null desc then "OPointTriangleConsistency" else desc

angleConsistency' :: (Network [String] (ARel Otop) -> Maybe Formula)
                  -> Network [String] (ARel Otop)
                  -> Maybe Bool
angleConsistency' fun net@Network{nCons = cons, nDesc = desc} =
    maybe (Just False)
          (\f -> if yicesSat (str f) then Nothing else Just False)
          formula
  where
    formula = fun net
    str f = preamble desc f ++ ":formula\n" ++ showSMT f ++ "\n)"
--    str2 f = unsafePerformIO $ do                      --DEBUG
--        appendFile "BENCHMARK.EQUATIONS" (str f)       --DEBUG
--        return (str f)                                 --DEBUG

triangleConsistency =
    angleConsistency' (translateToTriangles False False)
triangleConsistencyWithWitness =
    angleConsistency' (translateToTriangles True False)
triangleConsistencyWithWitnessAndWitnesses =
    angleConsistency' (translateToTriangles True True)
triangleConsistencyWithWitnesses =
    angleConsistency' (translateToTriangles False True)

angleConsistency =
    angleConsistency' (translateToTrianglesOrig False False)
angleConsistencyWithWitness =
    angleConsistency' (translateToTrianglesOrig True False)
angleConsistencyWithWitnessAndWitnesses =
    angleConsistency' (translateToTrianglesOrig True True)
angleConsistencyWithWitnesses =
    angleConsistency' (translateToTrianglesOrig False True)

