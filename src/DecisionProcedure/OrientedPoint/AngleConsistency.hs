module DecisionProcedure.OrientedPoint.AngleConsistency where

import qualified Data.Foldable as Fold
import qualified Data.Key as Key
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace

-- this is for debugging:
--import System.IO.Unsafe

-- local modules
import Basics
import Helpful.General
import Interface.Yices
import SpatioTemporalStructure.OrientedPoint

-- maximal granularity for which this code should work.
maxgran = 10

halfcircle :: Integer
halfcircle = foldr1 lcm [1..maxgran]

fullcircle = halfcircle * 2

angle g s = quot (halfcircle * fromIntegral (quot s 2)) (fromIntegral g)

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


-- This function assumes, that the Otop relations are similar to the
-- corresponding opra relations. "Same" relations are mapped to a granularity
-- of 0 and then indicate the direction of the second opoint with respect to
-- the first.
-- Note: Add converses of same relations before giving the network to this
-- function in order to improve the reasoning.
-- improve: catch empty network.
-- improve: find a better way to preprocess the network.
translateToAngles :: Network [String] Otop -> Maybe Formula
translateToAngles net@Network{nCons = cons} = do
    let allNodes = nodesIn net
    let pairs    = kCombinations 2 $ Set.toAscList allNodes
    ps_u <- Fold.foldrM         -- pairsWithSameness_and_unknownPairs
        (\ pair@[node, node2] (acc, acc2, acc3) ->
          let 
            existsWitnessForUnSameness = not $ Set.null $ Set.filter
                (\ node3 ->
                  let
                    n3n  = Map.lookup [node3, node ] cons
                    n3n2 = Map.lookup [node3, node2] cons
                  in
                    node /= node3 && node2 /= node3 &&
                    isJust n3n && isJust n3n2 && n3n /= n3n2
                ) allNodes
            existWitnessesForSameness = not $ null $ filter
                (\ [node3, node4] ->
                  let
                    n3n  = Map.lookup [node3, node ] cons
                    n3n2 = Map.lookup [node3, node2] cons
                    n3n4 = Map.lookup [node3, node4] cons
                    n4n  = Map.lookup [node4, node ] cons
                    n4n2 = Map.lookup [node4, node2] cons
                    n4n3 = Map.lookup [node4, node3] cons
                    Otop gran_n3n  sec_n3n  = fromJust n3n
                    Otop gran_n3n2 sec_n3n2 = fromJust n3n2
                    Otop gran_n3n4 sec_n3n4 = fromJust n3n4
                    Otop gran_n4n  sec_n4n  = fromJust n4n
                    Otop gran_n4n2 sec_n4n2 = fromJust n4n2
                    Otop gran_n4n3 sec_n4n3 = fromJust n4n3
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
                ) pairs
            sameHelper g s g2 s2 = case sort [ (signum g , signum s )
                                             , (signum g2, signum s2)] of
                -- (-1) = same, 1 = not same, 0 = partial or no knowledge.
                [(-1, _), (-1, _)] -> newAccsTrue
                [( 1, _), ( 1, _)] -> newAccsFalse
                [(-1, _), ( 0, 1)] -> Nothing
                [(-1, _), ( 0, _)] -> newAccsTrue
                [( 0,-1), ( 1, _)] -> Nothing
                [( 0, _), ( 1, _)] -> newAccsFalse
                [( 0,-1), ( 0, 1)] -> Nothing
                [( 0,-1), ( 0, _)] -> newAccsTrue
                [( 0, _), ( 0, 1)] -> newAccsFalse
                [(-1, _), ( 1, _)] -> Nothing
            noInfoHelper = case ( existsWitnessForUnSameness
                                , existWitnessesForSameness  ) of
                (True , False) -> newAccsFalse
                (False, True ) -> newAccsTrue
                (False, False) -> newAccsUnknown
                (True , True ) -> Nothing
            newAccsTrue  = Just (Map.insert pair True  acc, newAcc2, acc3)
            newAccsFalse = Just (Map.insert pair False acc, newAcc2, acc3)
            newAccsUnknown = Just (acc, acc2, pair:acc3)
            newAcc2 = Map.insertWith Set.union node2 (Set.singleton node ) $
                      Map.insertWith Set.union node  (Set.singleton node2) acc2
          in
            -- improve:find more contradictions here by looking into g g2 s s2.
            -- e.g. same but angles dont match or g not negative.
            -- maybe even use witnesses and compare to given information.
-- THIS IS A SNIPPET I USED BEFORE.
--                        if (even s2 && angle (-g) s /= angle (-g2) s2) ||
--                           (odd s2 &&
--                               (angle (-g) s <= angle (-g2) (s2 - 1)) ||
--                               (angle (-g) s >= angle (-g2) (s2 + 1))    )
--                        then Nothing
            case ( Map.lookup [node, node2] cons
                 , Map.lookup [node2, node] cons ) of
                (Just (Otop 0 0), Just (Otop 0  0 )) -> noInfoHelper
                (Just (Otop g s), Just (Otop g2 s2)) -> sameHelper g s g2 s2
                (Just (Otop g s), Nothing)           -> sameHelper g s g s
                (Nothing, Just (Otop g s))           -> sameHelper g s g s
                (Nothing, Nothing)                   -> noInfoHelper
        ) (Map.empty, Map.empty, []) pairs
    let (pairsWithSameness, relatedNodes, unknownPairs) = ps_u
    translateToAngles' True cons pairsWithSameness
                                 pairsWithSameness relatedNodes unknownPairs

--    -- mapFromNodesToListsOfUnequalNodes
--    mfntloun = fst $ Set.foldr
--        (\ node (acc, acc2) ->
--          let
--            remNodes = Set.delete node acc2
--          in
--            ( Map.insert node
--                ( Set.partition
--                    -- (Set of nodes unequal to node, Set of all others)
--                    -- (but only those greater than node)
--                    (\ node2 -> case ( Map.lookup [node, node2] cons
--                                     , Map.lookup [node2, node] cons ) of
--                        (Just (Otop x _), _) -> x > 0
--                        (_, Just (Otop x _)) -> x > 0
--                        (Nothing, Nothing)   -> not $ Set.null $ Set.filter
--                            (\ node3 ->
--                              let
--                                n3n  = Map.lookup [node3, node ]
--                                n3n2 = Map.lookup [node3, node2]
--                              in
--                                node /= node3 && node2 /= node3 &&
--                                isJust n3n && isJust n3n2 && n3n /= n3n2
--                            ) allNodes
--                    ) $ Set.filter (node <) remNodes
--                ) acc
--            , remNodes
--            )
--        ) (Map.empty, allNodes) allNodes
--    -- mapFromNodesToListsOfUnequalEqAndUnknownNodes
--    mfntloueaun = Map.foldrWithKey
--        (\ node (uneqs, rest) acc ->
--          let
--            (equalNodes, unknowns) = Set.partition
--                (\ node2 -> case ( Map.lookup [node, node2] cons
--                                 , Map.lookup [node2, node] cons ) of
--                    (Just (Otop x _), _) -> x < 0
--                    (_, Just (Otop x _)) -> x < 0
--                    (Nothing, Nothing)   -> not $ null $ filter
--                        (\ [node3, node4] ->
--                          let
--                            n3n  = Map.lookup [node3, node ] cons
--                            n3n2 = Map.lookup [node3, node2] cons
--                            n3n4 = Map.lookup [node3, node4] cons
--                            n4n  = Map.lookup [node4, node ] cons
--                            n4n2 = Map.lookup [node4, node2] cons
--                            n4n3 = Map.lookup [node4, node3] cons
--                            Otop gran_n3n  sec_n3n  = fromJust n3n
--                            Otop gran n3n2 sec_n3n2 = fromJust n3n2
--                            Otop gran_n3n4 sec_n3n4 = fromJust n3n4
--                            Otop gran_n4n  sec_n4n  = fromJust n4n
--                            Otop gran_n4n2 sec_n4n2 = fromJust n4n2
--                            Otop gran_n4n3 sec_n4n3 = fromJust n4n4
--                          in
--                            isJust n3n && isJust n3n2 && isJust n4n && isJust n4n2
--                            && even sec_n3n && even sec_n4n
--                            -- n and n2 lie on the same line
--                            && (sec_n3n * gran_n3n2 == sec_n3n2 * gran_n3n)
--                            && (sec_n4n * gran_n4n2 == sec_n4n2 * gran_n4n)
--                            -- n3 and n4 not in line with n and n2
--                            && (  (isJust n3n4
--                                  && (sec_n3n * gran_n3n4 /= sec_n3n4 * gran_n3n)
--                                  && (sec_n3n * gran_n3n4 /=
--                                       mod (sec_n3n4 + 2 * gran_n3n4) (4 * gran_n3n4)
--                                       * gran_n3n))
--                               || (isJust n4n3
--                                  && (sec_n4n * gran_n4n3 /= sec_n4n3 * gran_n4n)
--                                  && (sec_n4n * gran_n4n3 /=
--                                       mod (sec_n4n3 + 2 * gran_n4n3) (4 * gran_n4n3)
--                                       * gran_n4n))
--                               )
--                        ) pairs
--                ) rest
--          in
--            Map.insert node (uneqs, equalNodes, unknowns) acc
--        ) Map.empty mfntloun


translateToAngles' :: Bool
                   -> Map.Map [String] Otop
                   -> Map.Map [String] Bool
                   -> Map.Map [String] Bool
                   -> Map.Map  String  (Set.Set String)
                   -> [[String]]
                   -> Maybe Formula
translateToAngles' firstRun
                   cons newPairs pairsToRelateTo relatedNodes unknownPairs =
    if null unknownPairs then
        equNewPairs
    else do
        a <- equNewPairs
        b <- equUnknownPairs
        Just $ And a b
  where
    equNewPairs = Key.foldrWithKeyM
        (\ pair@[node, node2] same acc -> do
          let
            revPair = [node2, node]
            varPair = Var pair
            varRevPair = Var revPair
            noInfoSame =
                And (Leq (Con 0) varPair) (Le varPair (Con fullcircle))
            noInfoNotSame = And
                (And (Leq (Con 0) varPair   )
                     (Le varPair    (Con fullcircle)))
                (And (Leq (Con 0) varRevPair)
                     (Le varRevPair (Con fullcircle)))
            fstSame g s = equKnownVar varPair (-g) s
            sndSame g s =
                if even s then
                    Eq varPair (Con $ mod (-angle (-g) s) fullcircle)
                else
                    And (Le (Con $ mod (-angle (-g) (s + 1)) fullcircle)
                            varPair)
                        (Le varPair
                            (Con $ mod (-angle (-g) (s - 1)) fullcircle))
            fstNotSame g s = notSame varPair varRevPair g s
            sndNotSame g s = notSame varRevPair varPair g s
            notSame knownVar unknownVar g s = And
                (And (Leq (Con 0) unknownVar)
                     (Le unknownVar (Con fullcircle))) $
                equKnownVar knownVar g s
            equKnownVar var g s =
                if even s then
                    Eq var (Con $ angle g s)
                else
                    And (Le (Con $ angle g (s - 1)) var)
                        (Le var (Con $ angle g (s + 1)))
            equPair = case ( Map.lookup pair cons
                           , Map.lookup revPair cons
                           , same
                           ) of
                (Just (Otop 0 _), Just (Otop 0  _ ), True ) -> noInfoSame
                (Just (Otop 0 _), Just (Otop 0  _ ), False) -> noInfoNotSame
                (Just (Otop 0 _), Just (Otop g  s ), True ) -> sndSame    g s
                (Just (Otop 0 _), Just (Otop g  s ), False) -> sndNotSame g s
                (Just (Otop g s), Just (Otop 0  _ ), True ) -> fstSame    g s
                (Just (Otop g s), Just (Otop 0  _ ), False) -> fstNotSame g s
                (Just (Otop g s), Just (Otop g2 s2), True ) -> 
                    if even s then
                        Eq varPair $ Con $ angle (-g) s
                    else if even s2 then
                        Eq varPair (Con $ mod (-angle (-g2) s2) fullcircle)
                    else
                        And (Le (Con $ max (angle (-g) (s - 1))
                                           (mod (-angle (-g2) (s2 + 1))
                                                fullcircle ))
                                varPair )
                            (Le varPair
                                (Con $ min (angle (-g) (s + 1))
                                           (mod (-angle (-g2) (s2 - 1))
                                                fullcircle)))
                (Just (Otop g s), Just (Otop g2 s2), False) ->
                    And (equKnownVar varPair g s)
                        (equKnownVar varRevPair g2 s2)
                (Just (Otop g s), Nothing          , True ) -> fstSame    g s
                (Just (Otop g s), Nothing          , False) -> fstNotSame g s
                (Nothing, Just (Otop g s)          , True ) -> sndSame    g s
                (Nothing, Just (Otop g s)          , False) -> sndNotSame g s
                (Nothing, Nothing                  , True ) -> noInfoSame
                (Nothing, Nothing                  , False) -> noInfoNotSame
            -- improve: we could detect some more inconsistencies here.
            -- e.g. by checking whether the third triple is also same when the
            -- other two are.
          equTriples <- Fold.foldrM
              (\ node3 acc2 -> do
                let
                  pair2 = sort [node , node3]
                  pair3 = sort [node2, node3]
                  thirdNode a = head $ [node, node2, node3]\\a
                  startToThird [a,b] = [a, thirdNode [a,b]]
                  endToThird   [a,b] = [b, thirdNode [a,b]]
                  triple [a,b] = [a, thirdNode [a,b], b]
                  equUnsameTriple a@[a1,a2] b@[b1,b2] c@[c1,c2] =
                      And (
                      And (
                      And
                      (Eq (Var $ triple a)
                          (Fo $ Ite (Leq (Var [thirdNode a, a1])
                                         (Var [thirdNode a, a2]))
                                    (Te $ Sub (Var [thirdNode a, a2])
                                              (Var [thirdNode a, a1]))
                                    (Te $ Add (Var [thirdNode a, a1])
                                              (Sub (Con fullcircle)
                                                   (Var [thirdNode a, a2])))))
                      (Eq (Var $ triple b)
                          (Fo $ Ite (Leq (Var [thirdNode b, b1])
                                         (Var [thirdNode b, b2]))
                                    (Te $ Sub (Var [thirdNode b, b2])
                                              (Var [thirdNode b, b1]))
                                    (Te $ Add (Var [thirdNode b, b1])
                                              (Sub (Con fullcircle)
                                                   (Var [thirdNode b, b2]))))))
                      (Eq (Var $ triple c)
                          (Fo $ Ite (Leq (Var [thirdNode c, c1])
                                         (Var [thirdNode c, c2]))
                                    (Te $ Sub (Var [thirdNode c, c2])
                                              (Var [thirdNode c, c1]))
                                    (Te $ Add (Var [thirdNode c, c1])
                                              (Sub (Con fullcircle)
                                                   (Var [thirdNode c, c2]))))))
                      (Eq (Add (Var $ triple a) $ Add (Var $ triple b)
                                                      (Var $ triple c))
                          (Con halfcircle))
                helper <- case sort [ (same, pair)
                                    , (pairsToRelateTo Map.! pair2, pair2)
                                    , (pairsToRelateTo Map.! pair3, pair3)
                                    ] of
                  -- improve: use sectors to check whether we need "Ite".
                  -- This replaces the "ite"s.
                  -- this would dramatically improve this method.
                      [(False, a), (False, b), (False, c)] -> Just $
                          Ite (Or (And (Leq (Con 0)
                                            (Sub (Var b) (Var a)))
                                       (Leq (Sub (Var b) (Var a))
                                            (Con halfcircle)))
                                  (And (Leq (Con 0)
                                            (Add (Sub (Var b) (Var a))
                                                 (Con fullcircle)))
                                       (Leq (Add (Sub (Var b) (Var a))
                                                 (Con fullcircle))
                                            (Con halfcircle))))
                              (equUnsameTriple c (reverse b) a)
                              (equUnsameTriple (reverse c) b (reverse a))
                      [(False, _), (False, _), (True , c)] -> Just $
                          (Eq (Var $ startToThird c)
                              (Fo $ Ite (Le (Add (Var c) (Var $ endToThird c))
                                            (Con fullcircle))
                                        (Te $ Add (Var c) (Var $ endToThird c))
                                        (Te $ Sub (Add (Var c)
                                                       (Var $ endToThird c))
                                                  (Con fullcircle))))
                      [(True , a), (True , b), (True , c)] -> Just $
                         -- improve: use sectors to check whether we need "Ite"
                          (Eq (Var b)
                              (Fo $ Ite (Le (Add (Var a) (Var c))
                                            (Con fullcircle))
                                        (Te $ Add (Var a) (Var c))
                                        (Te $ Sub (Add (Var a) (Var c))
                                                  (Con fullcircle)) ))
                      [(False, a), (True , b), (True , c)] -> Nothing
                Just $ And acc2 helper
              ) acc $ (if firstRun then Set.filter (> node2) else id) $
                      Set.intersection (relatedNodes Map.! node )
                                       (relatedNodes Map.! node2)
          Just $ And equPair equTriples
        ) Tru newPairs
    equUnknownPairs = do
        a <- equUnknownPairs' True
        b <- equUnknownPairs' False
        Just $ Or a b
    equUnknownPairs' s =
        translateToAngles' False cons (Map.singleton newPair s)
                                      (Map.union newPairs pairsToRelateTo)
                                      (Map.insertWith
                                          Set.union b (Set.singleton a) $
                                          Map.insertWith
                                              Set.union
                                              a (Set.singleton b)
                                              relatedNodes)
                                      remUnknownPairs
    (newPair@[a,b]:remUnknownPairs) = unknownPairs


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


preamble :: String -> Formula -> String
preamble desc f = "(benchmark " ++ desc ++ "\n\n" ++
--    ":logic QF_AUFLIA\n" ++
    ":logic QF_LRA\n" ++ ":extrafuns (\n" ++ Set.foldr
    (\ v acc -> acc ++ "  (" ++ showSMT v ++ " Real)\n"
    ) "" (getVarsFo f) ++ ")\n"

parseOutputFromYices :: String
                     -> Maybe Bool
parseOutputFromYices str =
  let
    sat   = or $ map (\x -> "sat"   `isPrefixOf` x) $ lines str
    unsat = or $ map (\x -> "unsat" `isPrefixOf` x) $ lines str
  in
    case (sat, unsat) of
        (True, False) -> Nothing
        (False, True) -> Just False
        (_, _)        -> error $ "Help! Yices answered:\n" ++ str

angleConsistency :: Network [String] Otop -> Maybe Bool
angleConsistency net@Network{nCons = cons, nDesc = desc} =
    if isNothing maybeFormula then Just False else
    parseOutputFromYices $ readYices str
-- this is for debugging:
--    unsafePerformIO (writeFile "test.smt" str >> (return $ parseOutputFromYices $ readYices str))
  where
    maybeFormula = translateToAngles net
    formula = fromJust maybeFormula
    str = preamble desc formula ++ ":formula\n" ++ showSMT formula ++ "\n)"

