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

-- let (ReferentialType content references') = increaseReferences contentReferentialType $ length references

assertType :: Type -> Value -> ParserWithDoubleState [Type] Program ()
assertType _ (CompilerDefined _) = return ()
assertType (TypeReference index) content = do
    frame <- gets (!! index)
    case frame of
        (ForAllInstances typeClassNames) -> undefined
        _ -> assertType frame content
-- mapM_ assertType arguments
assertType frame content@(DefinedValue _ name) = do
    superiorReferentialType <- lift $ getTypeFromName name
    superiorType <- addReferentialType superiorReferentialType
    success <- typeGreaterThan superiorType frame
    unless success $ failTypeInference frame content
assertType (AliasReference _ name arguments) content = do
    alias <- lift $ getAlias name
    if name == "Id"
        then case arguments of
            [inputType, outputType] -> assertArrow inputType outputType content
            _ -> undefined
        else do
            increasedMainType <- addAlias arguments alias
            -- let ReferentialType unincreasedMainType otherTypesWithPlaceholders = aliasType alias
            -- let otherTypesWithoutPlaceholders = drop (length otherTypesWithPlaceholders - length arguments) otherTypesWithPlaceholders
            -- amount <- gets $ subtract (length arguments) . length
            -- let (ReferentialType increasedMainType newReferences) = increaseReferences (ReferentialType unincreasedMainType otherTypesWithoutPlaceholders) amount
            -- gets (++ arguments ++ newReferences) >>= put
            assertType increasedMainType content
-- gets (head . filter (\alias ->) . aliases)
--  case content of
--    (AliasReference _ name' contentTypes) -> do
--        when (name /= name' || length frameTypes /= length contentTypes) $ failTypeInference frame content
--        zipWithM_ assertType frameTypes contentTypes
assertType (AliasExtention index arguments) content = do
    (AliasReference offset name arguments') <- gets (!! index)
    assertType (AliasReference offset name (arguments' ++ arguments)) content
assertType (AnyType offset _) _ = undefined
assertType (ForAllInstances _) _ = undefined
assertType frame@(Sum x y) content = case content of
    (SumLiteral _ x' y') -> assertType x x' >> assertType y y'
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

-- assertType (AnyType index) content = case content of
--    (BoolLiteral _ _) -> modify (replace index Bool)
--    (IntLiteral _ _) -> modify (replace index Int)
--    (FloatLiteral _ _) -> modify (replace index Float)
--    (EmptyTupleLiteral _) -> modify (replace index EmptyTuple)
--    (CharLiteral _ _) -> modify (replace index Char)
--    _ -> return ()

assertArrow :: Type -> Type -> Value -> ParserWithDoubleState [Type] Program ()
assertArrow inputType outputType content = case content of
    (ArrowConstant _ value) -> assertType outputType value
    (ArrowComposition _ firstArrow secondArrow) -> do
        sharedType <- addTypeVariabel
        -- mapM_ (\(Definition _ referentialType value) -> assertReferentialType (referentialType value) allDefinitions)
        assertType (AliasReference (getOffsetFromValue firstArrow) "Id" [inputType, sharedType]) firstArrow
        assertType (AliasReference (getOffsetFromValue secondArrow) "Id" [sharedType, outputType]) secondArrow
    (ArrowFirst offset innerArrow) -> do
        bypassedType <- addTypeVariabel
        innerInputType <- addTypeVariabel
        innerOutputType <- addTypeVariabel
        successInput <- typeGreaterThan (Product innerInputType bypassedType) inputType
        unless successInput $ failTypeMatch offset (Product innerInputType bypassedType) inputType
        successOutput <- typeGreaterThan (Product innerOutputType bypassedType) outputType
        unless successOutput $ failTypeMatch offset (Product innerOutputType bypassedType) outputType
        assertType (AliasReference (getOffsetFromValue innerArrow) "Id" [innerInputType, innerOutputType]) innerArrow
    -- (ArrowSecond _ arrow) -> return ()
    -- (TripleAsterisks _ firtsArrow secondArrow) -> return ()
    -- (TripleAnd _ firstArrow secondArrow) -> return ()
    -- (ArrowRight _ arrow) -> return ()
    -- (ArrowLeft _ arrow) -> return ()
    -- (TriplePlus _ leftArrow rightArrow) -> return ()
    -- (TripleBar _ leftArrow rightArrow) -> return ()
    _ -> return ()

--
{-    data Value
    = ProductLiteral Int Value Value
    | SumLiteral Int Value Value
    | BoolLiteral Int Bool
    | FloatLiteral Int Double
    | IntLiteral Int Int
    | CharLiteral Int Char
    | EmptyTupleLiteral Int
    | DefinedValue Int String
    | ArrowComposition Int Value Value
    | ArrowConstant Int Value
    | ArrowFirst Int Value
    | ArrowSecond Int Value
    | TripleAsterisks Int Value Value
    | TripleAnd Int Value Value
    | ArrowRight Int Value
    | ArrowLeft Int Value
    | TriplePlus Int Value Value
    | TripleBar Int Value Value
    | CompilerDefined Int
    deriving (Read, Eq, Ord, Show)

-}

failTypeInference :: Type -> Value -> ParserWithDoubleState [Type] Program ()
failTypeInference frame content = do
    setOffset (getOffsetFromValue content)
    currentState <- get
    program <- lift get
    fail $ "could not match " ++ show frame ++ " with " ++ show content ++ "\n\n" ++ show currentState ++ "\n\n" ++ show program

failTypeMatch :: ParsingOffset -> Type -> Type -> ParserWithDoubleState [Type] Program ()
failTypeMatch offset superior inferior = do
    setOffset offset
    fail $ "could not match " ++ show superior ++ " with " ++ show inferior

-- tryDereference :: Type -> ParserWithState [Type] Type
-- tryDereference (TypeReference index) = gets (!! index)
-- tryDereference x = return x

-- merge :: ReferentialType -> ReferentialType -> Int -> ReferentialType
-- merge (ReferentialType parent references) child index = ReferentialType parent (references ++ childReferences)
--  where
--    (ReferentialType innerChild childReferences) = increaseReferences child $ length references
--    references' = references !! index

-- typeGreaterThan :: ReferentialType -> ReferentialType -> Maybe String
-- typeGreaterThan (ReferentialType greater referencesGreater) (ReferentialType smaller referencesSmaller) = undefined

typeGreaterThan :: Type -> Type -> ParserWithDoubleState [Type] Program Bool
typeGreaterThan (TypeReference index) smaller = do
    larger <- gets (!! index)
    typeGreaterThan larger smaller
typeGreaterThan larger (TypeReference index) = do
    smaller <- gets (!! index)
    typeGreaterThan larger smaller
typeGreaterThan (AliasReference _ largerName largerArguments) (AliasReference _ smallerName smallerArguments) =
    if largerName == smallerName
        then
            and <$> zipWithM typeGreaterThan largerArguments smallerArguments
        else return False
typeGreaterThan (ForAllInstances requiredClassesWithOffsets) (AnyType index existingClassesWithOffsets) = do
    -- let requredClasses = map snd requiredClassesWithOffsets
    -- let existingClasses = map snd existingClassesWithOffsets
    modify $ replace index $ AnyType index $ unionBy (\x y -> snd x == snd y) requiredClassesWithOffsets existingClassesWithOffsets
    return True
typeGreaterThan larger (AnyType index classesWithOffsets) = do
    -- TODO larger must have instances of all classes
    modify $ replace index larger
    return True
typeGreaterThan (ForAllInstances classesWithOffsets) smaller = do
    smallerReferentialType <- gets $ ReferentialType smaller
    let classNames = map snd classesWithOffsets
    instancesOfTypeNames <- lift $ gets $ map instanceClassName . filter ((== smallerReferentialType) . instanceType) . instances
    return $ all (`elem` instancesOfTypeNames) classNames

-- let allInstanceAliasNames = nub $ map instanceAliasName allInstances
-- let allInstanceAliasNamesWithClasses = map (\name -> (name, map typeClassNameOf $ filter ((== name) . instanceAliasName) allInstances)) allInstances
-- let instanceAliasNamesOfClasses = map fst $ filter (\(_, classesOfAlias) -> all (`elem` classesOfAlias) classes) allInstanceAliasNamesWithClasses
-- aliasesOfClasses <- lift $ mapM getAlias instanceAliasNamesOfClasses
-- or <$> mapM (\name -> typeGreaterThan (AliasReference 0 name arguments) smaller) instanceAliasNamesOfClasses
typeGreaterThan larger smaller = return $ larger == smaller

--  = Product Type Type
--    | Sum Type Type
--    | Bool
--    | Float
--    | Int
--    | Char
--    | EmptyTuple
--    | InstanceOf [(Int, String)]
--    | AliasReference Int String [Type]
--    | TypeReference Int

addAlias :: [Type] -> TypeAlias -> ParserWithDoubleState [Type] Program Type
addAlias arguments (TypeAlias _ _ (ReferentialType unincreasedMainType otherTypesWithPlaceholders)) = do
    -- insertedReferentialType <- aliasType <$> (lift $ getAlias name)
    -- outerReferentialType <- gets
    -- zipWithM increaseReferences arguments [0..]
    -- ReferentialType unincreasedMainType otherTypesWithPlaceholders <- aliasType <$> lift (getAlias name)
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
