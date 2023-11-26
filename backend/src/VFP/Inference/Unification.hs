{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use <$>" #-}

module VFP.Inference.Unification(
    UnificationType(..),
    typeContainsVariable,
    TypeConstraint,
    TypeConstraintConjunction,
    ResolvedTypes,
    unification) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy
import Control.Monad
import Data.Tuple (swap)

import Debug.Trace

{-
Unregularities in SPJ slides:

- Definition: https://youtu.be/x3evzO8O9e8?t=1829
- Example: https://youtu.be/x3evzO8O9e8?t=1980

-- 1. Class constraints are single constaints:
   In the proposed syntax, a class constrained can't be compared to another type constraint

-- 2. Type Constaints are supposed to be flat (F) or trees (W)
   In the proposed syntax: F ::= F1, F2 is always a tree for more than 2 contraints, and by definition not flat 
   (as well as undecidable)
-}

data UnificationType = UnificationVariable String
                     | UnificationConstantType String
                     | UnificationConstructedType String [UnificationType]
                     deriving (Eq, Ord)

instance Show UnificationType where
    show (UnificationVariable name) = name
    show (UnificationConstantType name) = name
    show (UnificationConstructedType tcname tname) = tcname ++ " " ++ show tname

type TypeConstraint = (UnificationType, UnificationType)
type TypeConstraintConjunction = Set.Set TypeConstraint

typeContainsVariable :: UnificationType -> Bool
typeContainsVariable (UnificationConstantType _) = False
typeContainsVariable (UnificationVariable _) = True
typeContainsVariable (UnificationConstructedType _ query) = any typeContainsVariable query

typeIsVariable :: UnificationType -> Bool
typeIsVariable (UnificationVariable _) = True
typeIsVariable _ = False

countVariables :: UnificationType -> Int
countVariables typ = length $ Set.toList $ getVariables typ
    where
        getVariables :: UnificationType -> Set.Set UnificationType
        getVariables (UnificationConstantType _) = Set.empty
        getVariables (UnificationVariable v) = Set.singleton $ UnificationVariable v
        getVariables (UnificationConstructedType _ args) = Set.unions $ map getVariables args

substituteType :: UnificationType -> UnificationType -> UnificationType -> UnificationType
substituteType from to (UnificationVariable other) = if UnificationVariable other == from then to else UnificationVariable other
substituteType from to (UnificationConstantType other) = if UnificationConstantType other == from then to else UnificationConstantType other
substituteType from to (UnificationConstructedType tClass other) = UnificationConstructedType tClass $ map (substituteType from to) other

type ResolvedTypes = Map.Map UnificationType UnificationType

data UnificationStateValue = UnificationState { constraints :: TypeConstraintConjunction, resolvedTypes :: ResolvedTypes, deletedConstraints :: TypeConstraintConjunction }
    deriving (Eq, Show)

type UnificationState = State UnificationStateValue

initialStateValue :: TypeConstraintConjunction -> UnificationStateValue
initialStateValue cs = UnificationState{constraints=cs, resolvedTypes = Map.empty, deletedConstraints = Set.empty}

mapConstraints :: (TypeConstraintConjunction -> TypeConstraintConjunction) -> UnificationState ()
mapConstraints mapping = do
    current <- get
    put current{constraints = mapping $ constraints current}

mapResolvedTypes :: (ResolvedTypes -> ResolvedTypes) -> UnificationState ()
mapResolvedTypes mapping = do
    current <- get
    put current{resolvedTypes = mapping $ resolvedTypes current}

getResolvedType :: UnificationType -> UnificationState (Maybe UnificationType)
getResolvedType search = do
    current <- get
    return $ Map.lookup search $ resolvedTypes current

deleteConstraint :: TypeConstraint -> UnificationState ()
deleteConstraint c = mapConstraints (Set.delete c)

addConstraint :: TypeConstraint -> UnificationState ()
addConstraint c = mapConstraints (Set.insert c)

addConstraints :: [(UnificationType, UnificationType)] -> UnificationState ()
addConstraints cs = mapConstraints (foldr Set.insert $ Set.fromList cs)

addResolvedType :: UnificationType -> UnificationType -> UnificationState ()
addResolvedType from to = do
    current <- getResolvedType from
    case current of
        Nothing -> addGivenResolve
        Just cur -> do
            when (countVariables cur > countVariables to) $ do
                mapResolvedTypes (Map.delete cur)
                addGivenResolve

            when (typeContainsVariable to && typeContainsVariable cur) $ do
                addConstraint (to, cur)
    where
        addGivenResolve :: UnificationState ()
        addGivenResolve = do
            mapConstraints $ Set.map (\(x,y) -> (substituteType from to x, substituteType from to y))
            mapResolvedTypes $ Map.insert from to

{-
Unification algorithm, as inspired by Metha, PROGRAMMING IN PROLOG UNIFICATION AND PROOF SEARCH, page 11
-}

clearResolveds :: UnificationState ()
clearResolveds = do
    cur <- get
    let resolveds = Map.toList $ resolvedTypes cur
    mapM_ (\(from, to) -> mapResolvedTypes $ Map.map (substituteType from to)) resolveds
    mapM_ (\(from, to) -> mapConstraints $ Set.map (\(x,y) -> (substituteType from to x, substituteType from to y))) resolveds

processConstraint :: TypeConstraint -> UnificationState ()
processConstraint (leftC, rightC) = do
    let constraint = (leftC, rightC)
    if leftC == rightC
        then do deleteConstraint constraint
        else case constraint of
            (UnificationConstantType _, UnificationConstantType _) ->
                when (leftC == rightC) $ deleteConstraint constraint
            (UnificationVariable _, UnificationVariable _) -> do
                deleteConstraint constraint
                addResolvedType leftC rightC
            (UnificationVariable _, _) ->
                unless (typeContainsVariable rightC) $ do
                    deleteConstraint constraint
                    addResolvedType leftC rightC
            (_, UnificationVariable _) -> do
                deleteConstraint constraint
                addConstraint (rightC, leftC)
            (UnificationConstructedType leftN leftTs, UnificationConstructedType rightN rightTs) ->
                when (leftN == rightN && length leftTs == length rightTs) $ do
                    deleteConstraint constraint
                    addConstraints $ zip leftTs rightTs
            (_, _) -> return ()

processConstraints :: UnificationState ()
processConstraints = do
    s <- get
    let cs = Set.toList $ constraints s
    mapM_ processConstraint cs

augmentConstraints :: UnificationState ()
augmentConstraints = do
    s <- get
    let allConstraints = Set.toList $ constraints s
    let allConstraintsWithSwapped = allConstraints ++ map swap allConstraints
    let constraintsToAdd = [(snd left, snd right) |
                             left <- allConstraintsWithSwapped,
                             typeIsVariable $ fst left,
                             not $ typeIsVariable $ snd left,
                             right <- allConstraintsWithSwapped,
                             not $ typeIsVariable $ snd right,
                             fst left == fst right,
                             snd left /= snd right]
    addConstraints constraintsToAdd

untilStable :: UnificationState () -> UnificationState ()
untilStable action = do
    original <- get
    action
    next <- get
    unless (original == next) $ untilStable action

unifyUntilStable :: UnificationState ()
unifyUntilStable = untilStable $ do
    augmentConstraints
    clearResolveds
    untilStable processConstraints

unification :: TypeConstraintConjunction -> (TypeConstraintConjunction, ResolvedTypes)
unification initialConstraints =
    let result = execState unifyUntilStable $ initialStateValue initialConstraints
    in (constraints result, resolvedTypes result)
