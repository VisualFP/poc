{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use <$>" #-}

module VFP.Inference.Unification(
    UnificationType(..),
    TypeConstraint,
    TypeConstraintConjunction,
    ResolvedTypes,
    unification) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy
import Control.Monad

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

type ResolvedTypes = Map.Map UnificationType UnificationType

data UnificationStateValue = UnificationState { constraints :: TypeConstraintConjunction, resolvedTypes :: ResolvedTypes }
    deriving (Eq, Show)

type UnificationState = State UnificationStateValue

initialStateValue :: TypeConstraintConjunction -> UnificationStateValue
initialStateValue cs = UnificationState{constraints=cs, resolvedTypes=Map.empty}

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

addConstraints :: TypeConstraintConjunction -> UnificationState ()
addConstraints cs = mapConstraints (foldr Set.insert cs)

addResolvedType :: UnificationType -> UnificationType -> UnificationState ()
addResolvedType from to = do
    current <- getResolvedType from
    case current of
        Nothing -> addGivenResolve
        Just cur ->
            when (typeContainsVariable cur) $ do
                mapResolvedTypes (Map.delete cur)
                addGivenResolve
    where
        addGivenResolve :: UnificationState ()
        addGivenResolve = do
            mapConstraints $ Set.map (\(x,y) -> (substituteType x, substituteType y))
            mapResolvedTypes $ Map.insert from to
            mapResolvedTypes $ Map.map substituteType

        substituteType :: UnificationType -> UnificationType
        substituteType (UnificationVariable other) = if UnificationVariable other == from then to else UnificationVariable other
        substituteType (UnificationConstantType other) = if UnificationConstantType other == from then to else UnificationConstantType other
        substituteType (UnificationConstructedType tClass other) = UnificationConstructedType tClass $ map substituteType other

{-
Unification algorithm, as inspired by Metha, PROGRAMMING IN PROLOG UNIFICATION AND PROOF SEARCH, page 11
-}

processConstraint :: TypeConstraint -> UnificationState ()
processConstraint (leftC, rightC) = do
    let constraint = (leftC, rightC)
    if leftC == rightC
        then do deleteConstraint constraint
        else case constraint of
            (UnificationConstantType _, UnificationConstantType _) ->
                when (leftC == rightC) $ deleteConstraint constraint
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
                    addConstraints (Set.fromList $ zip leftTs rightTs)
            (_, _) -> return ()

unifyUntilStable :: UnificationState ()
unifyUntilStable = do
    original <- get
    let cs = Set.toList $ constraints original
    mapM_ processConstraint cs
    next <- get
    unless (original == next) unifyUntilStable

unification :: TypeConstraintConjunction -> (TypeConstraintConjunction, ResolvedTypes)
unification initialConstraints =
    let result = execState unifyUntilStable $ initialStateValue initialConstraints
    in (constraints result, resolvedTypes result)
