{-# LANGUAGE TupleSections #-}

module Checker.Types where

import Ast
import Control.Monad
import Control.Monad.State
import Data.List
import Helpers
import Parser.Primitives
import Text.Megaparsec hiding (State)

checkTypeSafety :: ParserWithState Program ()
checkTypeSafety = do
    allDefinitions <- gets definitions
    mapM_ (\(Definition _ referentialType value) -> assertReferentialType referentialType value) allDefinitions
    allDefinitionsFromInstances <- gets $ concatMap (\(Instance instanceType' _ members) -> map (instanceType',) members) . instances
    mapM_
        ( \(insertedType, (name, value)) -> do
            referentialType <- getTypeFromName name
            assertReferentialType (insertType referentialType insertedType 0) value
        )
        allDefinitionsFromInstances

assertReferentialType :: ReferentialType -> Value -> ParserWithState Program ()
assertReferentialType (ReferentialType frame references) content = void $ runStateT (assertType frame content) references

assertType :: Type -> Value -> ParserWithDoubleState [Type] Program ()
assertType _ (CompilerDefined _) = return ()
assertType (TypeReference index) content = do
    frame <- gets (!! index)
    case frame of
        (ForAllInstances typeClassNames) -> undefined
        _ -> assertType frame content
assertType frame content@(DefinedValue name) = do
    inferiorReferentialType <- lift $ toAnyTypeReferences <$> getTypeFromName name
    inferiorType <- addReferentialType inferiorReferentialType
    success <- typeGreaterThan frame inferiorType
    unless success $ failTypeInference frame content
assertType (AliasReference name arguments) content = do
    alias <- lift $ getAlias name
    if nameString name == "Id"
        then case arguments of
            [inputType, outputType] -> assertArrow inputType outputType content
            _ -> undefined
        else do
            increasedMainType <- addAlias arguments alias
            assertType increasedMainType content
assertType (AliasExtention index arguments) content = do
    (AliasReference name arguments') <- gets (!! index)
    assertType (AliasReference name (arguments' ++ arguments)) content
assertType (AnyType offset _) _ = undefined
assertType (ForAllInstances _) _ = undefined
assertType frame@(Sum x y) content = case content of
    (SumLiteral _ boolChoice value) -> if boolChoice then assertType y value else assertType x value
    _ -> failTypeInference frame content
assertType frame@(Product x y) content = case content of
    (ProductLiteral _ x' y') -> assertType x x' >> assertType y y'
    _ -> failTypeInference frame content
assertType frame@Bool content = case content of
    (BoolLiteral _ _) -> return ()
    _ -> failTypeInference frame content
assertType frame@Float content = case content of
    (FloatLiteral _ _) -> return ()
    _ -> failTypeInference frame content
assertType frame@Int content = case content of
    (IntLiteral _ _) -> return ()
    _ -> failTypeInference frame content
assertType frame@Char content = case content of
    (CharLiteral _ _) -> return ()
    _ -> failTypeInference frame content
assertType frame@EmptyTuple content = case content of
    (EmptyTupleLiteral _) -> return ()
    _ -> failTypeInference frame content

assertArrow :: Type -> Type -> Value -> ParserWithDoubleState [Type] Program ()
assertArrow inputType outputType content = case content of
    (ArrowConstant _ value) -> assertType outputType value
    (ArrowComposition _ firstArrow secondArrow) -> do
        sharedType <- addTypeVariabel
        assertType (AliasReference (Name (getOffsetFromValue firstArrow) "Id") [inputType, sharedType]) firstArrow
        assertType (AliasReference (Name (getOffsetFromValue secondArrow) "Id") [sharedType, outputType]) secondArrow
    (ArrowFirst offset innerArrow) -> do
        bypassedType <- addTypeVariabel
        innerInputType <- addTypeVariabel
        innerOutputType <- addTypeVariabel
        successInput <- typeGreaterThan (Product innerInputType bypassedType) inputType
        unless successInput $ failTypeMatch offset (Product innerInputType bypassedType) inputType
        successOutput <- typeGreaterThan (Product innerOutputType bypassedType) outputType
        unless successOutput $ failTypeMatch offset (Product innerOutputType bypassedType) outputType
        assertType (AliasReference (Name (getOffsetFromValue innerArrow) "Id") [innerInputType, innerOutputType]) innerArrow
    -- (ArrowSecond _ arrow) -> return ()
    -- (TripleAsterisks _ firtsArrow secondArrow) -> return ()
    -- (TripleAnd _ firstArrow secondArrow) -> return ()
    -- (ArrowRight _ arrow) -> return ()
    -- (ArrowLeft _ arrow) -> return ()
    -- (TriplePlus _ leftArrow rightArrow) -> return ()
    -- (TripleBar _ leftArrow rightArrow) -> return ()
    _ -> return ()

failTypeInference :: Type -> Value -> ParserWithDoubleState [Type] Program ()
failTypeInference frame content = do
    setOffset (getOffsetFromValue content)
    references <- get
    program <- lift get
    fail $ "could not match " ++ displayReferentialType (ReferentialType frame references) ++ " with " ++ show content ++ "\n\n" ++ show program

failTypeMatch :: ParsingOffset -> Type -> Type -> ParserWithDoubleState [Type] Program ()
failTypeMatch offset superior inferior = do
    setOffset offset
    fail $ "could not match " ++ show superior ++ " with " ++ show inferior

typeGreaterThan :: Type -> Type -> ParserWithDoubleState [Type] Program Bool
typeGreaterThan (TypeReference index) smaller = do
    larger <- gets (!! index)
    typeGreaterThan larger smaller
typeGreaterThan larger (TypeReference index) = do
    smaller <- gets (!! index)
    typeGreaterThan larger smaller
typeGreaterThan (AliasReference largerName largerArguments) (AliasReference smallerName smallerArguments) =
    if largerName == smallerName
        then
            and <$> zipWithM typeGreaterThan largerArguments smallerArguments
        else return False
typeGreaterThan (ForAllInstances requiredClasses) (AnyType index existingClasses) = do
    modify $ replace index $ AnyType index $ union requiredClasses existingClasses
    return True
typeGreaterThan larger (AnyType index classNames) = do
    isInstanceOfAllClasses <- and <$> mapM (isInstanceOf larger) classNames
    if isInstanceOfAllClasses
        then do
            modify $ replace index larger
            return True
        else
            return False
typeGreaterThan (AnyType index classNames) smaller = do
    isInstanceOfAllClasses <- and <$> mapM (isInstanceOf smaller) classNames
    if isInstanceOfAllClasses
        then do
            modify $ replace index smaller
            return True
        else
            return False
typeGreaterThan (ForAllInstances classNames) smaller = do
    smallerReferentialType <- gets $ ReferentialType smaller
    -- let classNames = map snd classesWithOffsets
    instancesOfTypeNames <- lift $ gets $ map instanceClassName . filter ((== smallerReferentialType) . instanceType) . instances
    return $ all (`elem` instancesOfTypeNames) classNames
typeGreaterThan larger smaller = return $ larger == smaller

isInstanceOf :: Type -> Name -> ParserWithDoubleState [Type] Program Bool
isInstanceOf (ForAllInstances classNames) className = return $ className `elem` classNames
isInstanceOf t className = do
    possibleReferentialTypes <- lift $ map instanceType <$> getInstances className
    or
        <$> mapM
            ( \possibleReferentialType -> do
                possibleType <- addReferentialType possibleReferentialType
                typeGreaterThan possibleType t
            )
            possibleReferentialTypes

addAlias :: [Type] -> TypeAlias -> ParserWithDoubleState [Type] Program Type
addAlias arguments (TypeAlias _ _ (ReferentialType unincreasedMainType otherTypesWithPlaceholders)) = do
    let otherTypesWithoutPlaceholders = drop (length otherTypesWithPlaceholders - length arguments) otherTypesWithPlaceholders
    amount <- gets $ subtract (length arguments) . length
    let (ReferentialType increasedMainType newReferences) = increaseReferences (ReferentialType unincreasedMainType otherTypesWithoutPlaceholders) amount
    gets (++ arguments ++ newReferences) >>= put
    return increasedMainType

addReferentialType :: ReferentialType -> ParserWithDoubleState [Type] Program Type
addReferentialType referentialType = do
    amount <- gets length
    let (ReferentialType result newReferences) = increaseReferences referentialType amount
    gets (++ newReferences) >>= put
    return result
