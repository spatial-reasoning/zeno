module DecisionProcedure.Opus.TriangleConsistency where

import qualified Data.Foldable as Fold
import qualified Data.Key as Key
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Ratio
import qualified Data.Set as Set

-- local modules
import Basics
import Helpful.General
import Helpful.TimeIt
import Interface.Yices
import qualified SpatioTemporalStructure.Interval as I
import SpatioTemporalStructure.OrientedPoint
import Calculus.Opra (angle, minAngle, maxAngle)


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


translateToTriangles :: Bool -> Bool -> Network [String] (Opus Rational)
                     -> Maybe Formula
translateToTriangles useWitness useWitnesses net@Network{nCons = cons'} = do
    let allNodes = Set.toAscList $ nodesIn $ nCons net
    let allIntervalEnds = Map.foldr
            (\Opus{ opusSame = os, opusNonSame = on } acc ->
                concatMap I.ends os ++ concatMap I.ends on ++ acc
            ) [] cons'
    let halfcircle = (foldr1 lcm . map denominator) allIntervalEnds
    let circle     = 2 * halfcircle
    -- make all angles Integral.
    let cons  = Map.map
            (\Opus{ opusSame = os, opusNonSame = on } -> Opus
                { opusSame    = map
                      (I.onEnds (numerator . ((toRational halfcircle) *))) os
                , opusNonSame = map
                      (I.onEnds (numerator . ((toRational halfcircle) *))) on }
            ) cons'
    let pairs = kCombinations 2 allNodes
    -- Fold over all pairs and generate:
    --   1. the map mapping pairs to either True or False depending on whether
    --      they are in a 'SAME' relation or not. Pairs for which we have no
    --      information are left out of the map.
    --   2. the map mapping nodes to the set of nodes of which we know whether
    --      the node lies in a 'SAME' relation to them or not. Nodes for which
    --      we have no information are left out of the map respectively the
    --      set.
    -- abbr.: pairsWithSameness_and_relatedNodes
    let noRestriction var = And (Leq (Con 0) var) (Le (Con circle) var)
    let equKnownPair var ivals = foldl1 Or $ map
            (\ ival -> And
                ((if I.infOpen ival then Le else Leq) (Con $ I.inf ival) var)
                ((if I.supOpen ival then Le else Leq) var (Con $ I.sup ival))
            ) ivals
    let unknownPair pair same nonSame = Or
            (And (Eq (Var $ pair ++ ["Same"]) (Con 0))
                 (And (equKnownPair (Var pair) nonSame)
                      (noRestriction (Var $ reverse pair))))
            (And (Eq (Var $ pair ++ ["Same"]) (Con 1))
                 (equKnownPair (Var pair) same))
    let unknownRevPair pair same nonSame = Or
            (And (Eq (Var $ pair ++ ["Same"]) (Con 0))
                 (And (noRestriction (Var pair))
                      (equKnownPair (Var $ reverse pair) nonSame)))
            (And (Eq (Var $ pair ++ ["Same"]) (Con 1))
                 (equKnownPair (Var pair) $ I.invertModuloList circle same))
    let unknownPairs pair ivalSame1 ivalNonSame1 ivalSame2 ivalNonSame2 = Or
            (And (Eq (Var $ pair ++ ["Same"]) (Con 0))
                 (And (equKnownPair (Var pair)           ivalNonSame1)
                      (equKnownPair (Var $ reverse pair) ivalNonSame2))
            )
            (And (Eq (Var $ pair ++ ["Same"]) (Con 1))
                 (equKnownPair (Var pair) $ I.intersections
                      ivalSame1 $ I.invertModuloList circle ivalSame2)
            )

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
                        Opus{ opusSame = s1, opusNonSame = u1 } = fromJust n3n
                        Opus{ opusSame = s2, opusNonSame = u2 } = fromJust n3n2
                        newAccSame = [accUnsame, True]
                        newAccUnsame = [True, accSame]
                        anglesDontMatch = null $ I.intersections u1 u2
                      in
                        if node /= node3 && node2 /= node3 &&
                           isJust n3n && isJust n3n2
                        -- improve: we can return a Maybe type in order to
                        -- propusgate an inconsistency.
                        then case sort [ [I.nullOnly s1, I.nullOnly u1]
                                       , [I.nullOnly s1, I.nullOnly u2] ] of
                            [[False, True ], [False, True ]] -> newAccSame
                            [[False, True ], [True , False]] -> newAccUnsame
                            [[True , False], [True , False]] ->
                                if anglesDontMatch then newAccUnsame else acc
                            otherwise -> acc
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
                        Opus{ opusSame = s_n3n , opusNonSame = u_n3n } =
                                                                fromJust n3n
                        Opus{ opusSame = s_n3n2, opusNonSame = u_n3n2} =
                                                                fromJust n3n2
                        Opus{ opusSame = s_n3n4, opusNonSame = u_n3n4} =
                                                                fromJust n3n4
                        Opus{ opusSame = s_n4n , opusNonSame = u_n4n } =
                                                                fromJust n4n
                        Opus{ opusSame = s_n4n2, opusNonSame = u_n4n2} =
                                                                fromJust n4n2
                        Opus{ opusSame = s_n4n3, opusNonSame = u_n4n3} =
                                                                fromJust n4n3
                      in
                        isJust n3n && isJust n3n2 &&
                        isJust n4n && isJust n4n2 &&
                        -- n3 and n4 are distinct from n and n2
                        I.nullOnly s_n3n && I.nullOnly s_n3n2 &&
                        I.nullOnly s_n4n && I.nullOnly s_n4n2 &&
                        -- n and n2 lie on the same line
                        -- when seen from n3 and n4
                        I.singularList u_n3n && I.singularList u_n4n &&
                        u_n3n == u_n3n2      && u_n4n == u_n4n2      &&
                        -- are n3 and n4 not in line with n and n2?
                        -- improve: we could account for the angles more
                        -- exactly and thus also test in the "same" case.
                        (  ( isJust n3n4 && I.nullOnly s_n3n4 &&
                             null (I.intersections u_n3n u_n3n4) &&
                             null (I.intersections
                                     (map (I.onEnds
                                           (\ x -> mod (x + halfcircle) circle)
                                          ) u_n3n
                                     ) u_n3n4
                                  )
                           )
                        || ( isJust n4n3 && I.nullOnly s_n4n3 &&
                             null (I.intersections u_n4n u_n4n3) &&
                             null (I.intersections
                                    (map (I.onEnds (\x ->
                                             mod (x + halfcircle) circle
                                                 )
                                         ) u_n4n)
                                    u_n4n3)
                           )
                        )
                    ) $ filter (null . intersect pair) pairs
              else
                False
          firstTestAnglesForSames a1 a2 =
              if null $ I.intersections a1 $ I.invertModuloList circle a2
              then
                  Nothing
              else
                  newAccsSame
          newAccsSame  =
              if foundWitnessForUnsameness then
                  Nothing
              else
                  newAccsSame'
          newAccsSame'  = Just (Map.insert pair (Just True) acc, newAcc2)
          newAccsUnsame =
              if foundWitnessForSameness || foundWitnessesForSameness then
                  Nothing
              else
                  newAccsUnsame'
          newAccsUnsame' = Just (Map.insert pair (Just False) acc, newAcc2)
          newAccsSomeInfo = Just (Map.insert pair Nothing acc, newAcc2)
          newAccsNoInfo = Just (acc, acc2)
          newAcc2 = Map.insertWith Set.union node (Set.singleton node2) acc2
          fewInfo nuAcc = case ( foundWitnessForUnsameness
                                     , foundWitnessForSameness ||
                                       foundWitnessesForSameness  ) of
              (True , False) -> newAccsUnsame'
              (False, True ) -> newAccsSame'
              (False, False) -> nuAcc
              (True , True ) -> Nothing
        in
          case sort [ Map.lookup [node, node2] cons
                    , Map.lookup [node2, node] cons ] of
              -- improve: here we might be able to use angular information.
              [ Just (Opus{ opusSame = s1, opusNonSame = u1 }) ,
                Just (Opus{ opusSame = s2, opusNonSame = u2 }) ] ->
                  case sort [ [I.nullOnly s1, I.nullOnly u1]
                            , [I.nullOnly s2, I.nullOnly u2] ] of
                      [[False, False], [False, False]] ->
                          if not $ null $ I.intersections s1 $
                              I.invertModuloList circle s2
                          then
                              fewInfo newAccsSomeInfo
                          else
                              newAccsUnsame
                      [[False, False], [False, True ]] -> newAccsSame
                      [[False, False], [True , False]] -> newAccsUnsame
                      [[False, True ], [False, True ]] -> newAccsSame
                      [[True , False], [True , False]] -> newAccsUnsame
                      otherwise -> Nothing
              [ Nothing, Just (Opus{ opusSame = s1, opusNonSame = u1 }) ] ->
                  case (I.nullOnly s1, I.nullOnly u1) of
                      (False, True ) -> newAccsSame
                      (True , False) -> newAccsUnsame
                      (False, False) -> fewInfo newAccsSomeInfo
                      (True , True ) -> Nothing
              otherwise -> fewInfo newAccsNoInfo
      ) (Map.empty, Map.empty) pairs
    let (pairsWithSameness, relatedNodes) = ps_rn
    -- Now translate into an equation.
    -- a foldrWithKeyM1 would be handy (and easy to implement)
    Key.foldrWithKeyM
      (\ pair@[node, node2] same acc -> do
        let
          revPair = [node2, node]
          varPair = Var pair
          varRevPair = Var revPair
          pairCons = Map.lookup pair cons
          revPairCons = Map.lookup revPair cons
          equPair = case (pairCons, revPairCons, same) of
              (Just x , Just y , Just False) -> And
                  (equKnownPair varPair    $ opusNonSame x)
                  (equKnownPair varRevPair $ opusNonSame y)
              (Just x , Just y , Just True ) -> equKnownPair varPair $
                  I.intersections (opusSame x) $ I.invertModuloList circle $
                                                                   opusSame y
              (Just x , Just y , Nothing   ) -> unknownPairs pair
                  (opusSame x) (opusNonSame x) (opusSame y) (opusNonSame y)
              (Just x , Nothing, Nothing   ) -> unknownPair pair
                                                   (opusSame x) (opusNonSame x)
              (Just x , Nothing, Just False) -> And
                                       (equKnownPair varPair $ opusNonSame x)
                                       (noRestriction varRevPair)
              (Just x , Nothing, Just True ) -> equKnownPair varPair $
                                                             opusSame x
              (Nothing, Just y , Just False) -> And
                                      (noRestriction varPair)
                                      (equKnownPair varRevPair $ opusNonSame y)
              (Nothing, Just y , Just True ) -> equKnownPair varPair $
                                         I.invertModuloList circle $ opusSame y
              (Nothing, Just y , Nothing   ) -> unknownRevPair pair
                                                   (opusSame y) $ opusNonSame y
              (Nothing, Nothing, Just False) -> And (noRestriction varPair)
                                                    (noRestriction varRevPair)
              (Nothing, Nothing, Just True ) -> noRestriction varPair
              (Nothing, Nothing, Nothing   ) -> error $
                  "DecisionProcedure.Opra.TriangleConsistency.equPair"

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
              equUnsameTriple tripleA tripleB tripleC = And (And (And
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
              allNonSame = Ite
                  (Or (And (Leq (Con 0)
                                (Sub (Var pair2) (Var pair)))
                           (Leq (Sub (Var pair2) (Var pair))
                                (Con halfcircle)))
                      (And (Leq (Con 0)
                                (Add (Var pair2)
                                     (Sub (Con circle)
                                          (Var pair))))
                           (Leq (Add (Var pair2)
                                     (Sub (Con circle)
                                          (Var pair)))
                                (Con halfcircle))))
                  (equUnsameTriple [node2, node , node3]
                                   [node3, node2, node ]
                                   [node , node3, node2])
                  (equUnsameTriple [node3, node , node2]
                                   [node , node2, node3]
                                   [node2, node3, node ])
              oneSame x = Eq (Var $ startToThird x)
                  (Fo $ Ite (Le (Add (Var x) (Var $ endToThird x))
                                (Con circle))
                            (Te $ Add (Var x) (Var $ endToThird x))
                            (Te $ Sub (Add (Var x)
                                           (Var $ endToThird x))
                                      (Con circle)))
              allSame = Eq (Var pair2)
                  (Fo $ Ite (Le (Add (Var pair) (Var pair3))
                                (Con circle))
                            (Te $ Add (Var pair) (Var pair3))
                            (Te $ Sub (Add (Var pair) (Var pair3))
                                      (Con circle)) )

            equTriple <- case sort [ (same, pair)
                                   , (pairsWithSameness Map.! pair2, pair2)
                                   , (pairsWithSameness Map.! pair3, pair3)
                                   ] of
                  [(Just False, _),
                   (Just False, _),
                   (Just False, _)] -> Just allNonSame

                  [(Just False, _),
                   (Just False, _),
                   (Just True , x)] -> Just $ oneSame x

                  [(Just True, ab),
                   (Just True, ac),
                   (Just True, bc)] -> Just allSame

                  [(Just False, _),
                   (Just True , _),
                   (Just True , _)] -> Nothing

                  [(Nothing   , x),
                   (Just False, _),
                   (Just False, _)] -> Just $ Or
                      (And (Eq (Var $ x ++ ["Same"]) (Con 0))
                           (allNonSame))
                      (And (Eq (Var $ x ++ ["Same"]) (Con 1))
                           (oneSame x))

                  -- Here "[a,b]" has to be nonsame:
                  -- nonsame(a,c) /\ same(b,c) => nonsame(a,b)
                  [(Nothing   , x),
                   (Just False, _),
                   (Just True , _)] -> Just $ And
                      (Eq (Var $ x ++ ["Same"]) (Con 1))
                      (oneSame x)

                  -- Here "[a,b]" has to be same:
                  -- same(a,c) /\ same(b,c) => same(a,b)
                  [(Nothing  , x),
                   (Just True, _),
                   (Just True, _)] -> Just allSame

                  [(Nothing   , x),
                   (Nothing   , y),
                   (Just False, _)] -> Just $ Or
                      (And (Eq (Var $ x ++ ["Same"]) (Con 0))
                           (Or (And (Eq (Var $ y ++ ["Same"]) (Con 0))
                                    (allNonSame))
                               (And (Eq (Var $ y ++ ["Same"]) (Con 1))
                                    (oneSame y))))
                      (And (Eq (Var $ x ++ ["Same"]) (Con 1))
                           (And (Eq (Var $ y ++ ["Same"]) (Con 0))
                                (oneSame x)))

                  [(Nothing  , x),
                   (Nothing  , y),
                   (Just True, z)] -> Just $ Or
                      (And (Eq (Var $ x ++ ["Same"]) (Con 0))
                           (And (Eq (Var $ y ++ ["Same"]) (Con 0))
                                (oneSame z)))
                      (And (Eq (Var $ x ++ ["Same"]) (Con 1))
                           (And (Eq (Var $ y ++ ["Same"]) (Con 1))
                                (allSame)))

                  [(Nothing, x),
                   (Nothing, y),
                   (Nothing, z)] -> Just $ Or
                      (And (Eq (Var $ x ++ ["Same"]) (Con 0))
                           (Or (And (Eq (Var $ y ++ ["Same"]) (Con 0))
                                    (Or (And (Eq (Var $ z ++ ["Same"]) (Con 0))
                                             (allNonSame))
                                        (And (Eq (Var $ z ++ ["Same"]) (Con 1))
                                             (oneSame z))))
                               (And (Eq (Var $ y ++ ["Same"]) (Con 1))
                                    (And (Eq (Var $ z ++ ["Same"]) (Con 0))
                                         (oneSame y)))))
                      (And (Eq (Var $ x ++ ["Same"]) (Con 1))
                           (Or (And (Eq (Var $ y ++ ["Same"]) (Con 0))
                                    (And (Eq (Var $ z ++ ["Same"]) (Con 0))
                                         (oneSame x)))
                               (And (Eq (Var $ y ++ ["Same"]) (Con 1))
                                    (And (Eq (Var $ z ++ ["Same"]) (Con 1))
                                         (allSame)))))
            Just $ And acc2 equTriple
          ) Tru $ Set.intersection
                      (maybe Set.empty id $ Map.lookup node  relatedNodes)
                      (maybe Set.empty id $ Map.lookup node2 relatedNodes)
        Just $ And acc $ And equPair equTriples
      ) Tru pairsWithSameness

preamble :: String -> Formula -> String
preamble desc f = "(benchmark " ++ desc' ++ "\n\n" ++
--    ":logic QF_AUFLIA\n" ++
    ":logic QF_LRA\n" ++ ":extrafuns (\n" ++ Set.foldl
    (\ acc v -> acc ++ "  (" ++ showSMT v ++ " Real)\n"
    ) "" (getVarsFo f) ++ ")\n"
  where
    desc' = if null desc then "OpusTriangleConsistency" else desc

triangleConsistency' :: (Network [String] (Opus Rational) -> Maybe Formula)
                     -> Network [String] (Opus Rational)
                     -> Maybe Bool
triangleConsistency' fun net@Network{nCons = cons, nDesc = desc} =
    maybe (Just False)
--          (\f -> if traceTime (yicesSat $ traceTime (str f)) then Nothing else Just False)   --DEBUGGING
          (\f -> if yicesSat (str f) then Nothing else Just False)
          formula
  where
    formula = fun net
    str f = preamble desc f ++ ":formula\n" ++ showSMT f ++ "\n)"

triangleConsistency =
    triangleConsistency' (translateToTriangles False False)
triangleConsistencyWithWitness =
    triangleConsistency' (translateToTriangles True False)
triangleConsistencyWithWitnessAndWitnesses =
    triangleConsistency' (translateToTriangles True True)
triangleConsistencyWithWitnesses =
    triangleConsistency' (translateToTriangles False True)

