module Checker.Types (assertReferentialType, checkTypeSafety) where

import Ast
import Control.Monad
import Control.Monad.State
import Data.Functor
import Helpers
import Parser.Primitives
import Text.Megaparsec hiding (State)

checkTypeSafety :: ParserWithState Program ()
checkTypeSafety = do
    allDefinitions <- gets definitions
    mapM_ checkDefinition allDefinitions
    -- modify $ \program -> program{definitions = zipWith (\(Definition name referentialType _) value -> Definition name referentialType value) allDefinitions definitionValues}
    -- allDefinitionsFromInstances <- gets instances >>= mapM getDefinitionsFromInstance
    -- instanceValues <- mapM (mapM (\(Definition _ referentialType value) -> assertReferentialType referentialType value)) allDefinitionsFromInstances
    -- modify $ \program -> program{instances = zipWith (\(Instance name referentialType _) value -> Definition name referentialType value) allDefinitionsFromInstances instanceValues}
    -- instances' <-
    --    gets instances
    --        >>= mapM
    gets instances
        >>= mapM_
            ( \currentInstance@(Instance t name members) -> do
                definitionsFromInstance <- getDefinitionsFromInstance currentInstance
                mapM_ checkDefinition definitionsFromInstance
                -- return $ Instance t name $ zip (map fst members) values
            )

-- modify $ \program -> program{instances = instances'}

-- mapM_ (uncurry assertReferentialType) allDefinition§sFromInstances

-- allDefinitionsFromInstances <- gets $ concatMap (\(Instance instanceType' _ members) -> map (instanceType',) members) . instances
-- mapM_
--    ( \(insertedType, (name, value)) -> do
--        referentialType <- getTypeFromName name
--        assertReferentialType (insertType referentialType insertedType 0) value
--    )
--    allDefinitionsFromInstances

checkDefinition :: Definition -> ParserWithState Program (TypeWithValue, [Type])
checkDefinition (Definition _ referentialType value) = assertReferentialType referentialType value

assertReferentialType :: ReferentialType -> Value -> ParserWithState Program (TypeWithValue, [Type])
assertReferentialType (ReferentialType frame references) content = runStateT (assertType frame content) references

assertType :: Type -> Value -> ParserWithDoubleState [Type] Program TypeWithValue
assertType frame (Undefined _) = return $ TypeWithUndefined frame
assertType (TypeReference index) content = do
    when (index < 0) $ fail $ show content
    frame <- gets (!! index)
    case frame of
        (ForAllInstances typeClassNames) -> undefined
        _ -> assertType frame content
assertType frame content@(DefinedValue name) = do
    eitherReferentialTypes <- lift $ getTypesFromName name
    case eitherReferentialTypes of
        (Left fromDefinition) -> do
            let inferiorReferentialType = toAnyTypeReferences fromDefinition
            inferiorType <- addReferentialType inferiorReferentialType
            typeGreaterThanWithOffset (nameOffset name) frame inferiorType
            return $ TypeWithDefinedValue frame name
        (Right fromInstances) -> do
            let inferiorReferentialTypes = map toAnyTypeReferences fromInstances
            possibleIndecies <-
                concat
                    <$> mapM
                        ( \(index, inferiorReferentialType) -> do
                            backup <- get
                            inferiorType <- addReferentialType inferiorReferentialType
                            (typeGreaterThanWithOffset (nameOffset name) frame inferiorType $> [index]) <|> (put backup >> return [])
                        )
                        (zip [0 ..] inferiorReferentialTypes)
            case possibleIndecies of
                [] -> failTypeInference frame content
                [index] -> return $ TypeWithDefinedValueFromInstance frame name index
                _ -> return $ TypeWithDefinedValue frame name
-- success <- typeGreaterThan frame inferiorType
-- unless success $ failTypeMatch (nameOffset name) frame inferiorType
assertType frame@(AliasReference name arguments) content = do
    alias <- lift $ getAlias name
    case alias of
        (TypeAlias _ _ Nothing) ->
            case nameString name of
                "Id" ->
                    case arguments of
                        [inputType, outputType] -> assertArrow "Id" inputType outputType content
                        _ -> undefined
                "IO" ->
                    case arguments of
                        [inputType, outputType] -> assertArrow "IO" inputType outputType content
                        _ -> undefined
                "Bool" -> case content of
                    (BoolLiteral _ value) -> return $ TypeWithBoolLiteral frame value
                    _ -> failTypeInference frame content
                "Float" -> case content of
                    (FloatLiteral _ value) -> return $ TypeWithFloatLiteral frame value
                    _ -> failTypeInference frame content
                "Int" -> case content of
                    (IntLiteral _ value) -> return $ TypeWithIntLiteral frame value
                    _ -> failTypeInference frame content
                "Char" -> case content of
                    (CharLiteral _ value) -> return $ TypeWithCharLiteral frame value
                    _ -> failTypeInference frame content
                "()" -> case content of
                    (EmptyTupleLiteral _) -> return $ TypeWithEmptyTupleLiteral frame
                    _ -> failTypeInference frame content
                _ -> undefined
        (TypeAlias _ _ (Just referentialType)) -> do
            increasedMainType <- addAlias arguments referentialType
            assertType increasedMainType content
assertType (AliasExtention index arguments) content = do
    (AliasReference name arguments') <- gets (!! index)
    assertType (AliasReference name (arguments' ++ arguments)) content
assertType (AnyType index []) content = do
    newType <- getTypeFromValue content
    modify $ replace index newType
    return $ addTypeToValue newType content
assertType (AnyType _ _) _ = undefined
assertType (ForAllInstances _) _ = undefined
assertType ThisClass _ = undefined
assertType frame@(Sum x y) content = case content of
    (SumLiteral _ boolChoice value) -> do
        result <- if boolChoice then assertType y value else assertType x value
        return $ TypeWithSumLiteral frame boolChoice result
    _ -> failTypeInference frame content
assertType frame@(Product x y) content = case content of
    (ProductLiteral _ x' y') -> do
        resultX <- assertType x x'
        resultY <- assertType y y'
        return $ TypeWithProductLiteral frame resultX resultY
    _ -> failTypeInference frame content

-- assertType frame@Bool content = case content of
--    (BoolLiteral _ _) -> return content
--    _ -> failTypeInference frame content
-- assertType frame@Float content = case content of
--     (FloatLiteral _ _) -> return content
--     _ -> failTypeInference frame content
-- assertType frame@Int content = case content of
--     (IntLiteral _ _) -> return content
--     _ -> failTypeInference frame content
-- assertType frame@Char content = case content of
--     (CharLiteral _ _) -> return content
--     _ -> failTypeInference frame content
-- assertType frame@EmptyTuple content = case content of
--     (EmptyTupleLiteral _) -> return content
--     _ -> failTypeInference frame content

assertArrow :: String -> Type -> Type -> Value -> ParserWithDoubleState [Type] Program TypeWithValue
assertArrow arrowInstance inputType outputType content =
    let arrowType = AliasReference (Name (getOffsetFromValue content) arrowInstance) [inputType, outputType]
     in case content of
            (ArrowConstant offset value) -> do
                value' <- assertType outputType value
                return $ TypeWithArrowConstant arrowType value'
            (ArrowComposition offset firstArrow secondArrow) -> do
                sharedType <- addTypeVariabel
                firstArrow' <- assertType (AliasReference (Name (getOffsetFromValue firstArrow) arrowInstance) [inputType, sharedType]) firstArrow
                secondArrow' <- assertType (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [sharedType, outputType]) secondArrow
                return $ TypeWithArrowComposition arrowType firstArrow' secondArrow'
            (ArrowFirst offset firstArrow) -> do
                bypassedType <- addTypeVariabel
                innerInputType <- addTypeVariabel
                innerOutputType <- addTypeVariabel
                typeGreaterThanWithOffset offset (Product innerInputType bypassedType) inputType
                -- successInput <- typeGreaterThan (Product innerInputType bypassedType) inputType
                -- unless successInput $ failTypeMatch offset (Product innerInputType bypassedType) inputType
                typeGreaterThanWithOffset offset (Product innerOutputType bypassedType) outputType
                -- successOutput <- typeGreaterThan (Product innerOutputType bypassedType) outputType
                -- unless successOutput $ failTypeMatch offset (Product innerOutputType bypassedType) outputType
                firstArrow' <- assertType (AliasReference (Name (getOffsetFromValue firstArrow) arrowInstance) [innerInputType, innerOutputType]) firstArrow
                return $ TypeWithArrowFirst arrowType firstArrow'
            (ArrowSecond offset secondArrow) -> do
                bypassedType <- addTypeVariabel
                innerInputType <- addTypeVariabel
                innerOutputType <- addTypeVariabel
                typeGreaterThanWithOffset offset (Product bypassedType innerInputType) inputType
                -- successInput <- typeGreaterThan (Product bypassedType innerInputType) inputType
                -- unless successInput $ failTypeMatch offset (Product bypassedType innerInputType) inputType
                typeGreaterThanWithOffset offset (Product bypassedType innerOutputType) outputType
                -- successOutput <- typeGreaterThan (Product bypassedType innerOutputType) outputType
                -- unless successOutput $ failTypeMatch offset (Product bypassedType innerOutputType) outputType
                secondArrow' <- assertType (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [innerInputType, innerOutputType]) secondArrow
                return $ TypeWithArrowSecond arrowType secondArrow'
            (TripleAsterisks offset firstArrow secondArrow) -> do
                firstInputType <- addTypeVariabel
                firstOutputType <- addTypeVariabel
                secondInputType <- addTypeVariabel
                secondOutputType <- addTypeVariabel
                typeGreaterThanWithOffset offset (Product firstInputType secondInputType) inputType
                -- successInput <- typeGreaterThan (Product firstInputType secondInputType) inputType
                -- unless successInput $ failTypeMatch offset (Product firstInputType secondInputType) inputType
                typeGreaterThanWithOffset offset (Product firstOutputType secondOutputType) outputType
                -- successOutput <- typeGreaterThan (Product firstOutputType secondOutputType) outputType
                -- unless successOutput $ failTypeMatch offset (Product firstOutputType secondOutputType) outputType
                firstArrow' <- assertType (AliasReference (Name (getOffsetFromValue firstArrow) arrowInstance) [firstInputType, firstOutputType]) firstArrow
                secondArrow' <- assertType (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [secondInputType, secondOutputType]) secondArrow
                return $ TypeWithTripleAsterisks arrowType firstArrow' secondArrow'
            (TripleAnd offset firstArrow secondArrow) -> do
                onlyInputType <- addTypeVariabel
                firstOutputType <- addTypeVariabel
                secondOutputType <- addTypeVariabel
                typeGreaterThanWithOffset offset onlyInputType inputType
                -- successInput <- typeGreaterThan onlyInputType inputType
                -- unless successInput $ failTypeMatch offset onlyInputType inputType
                typeGreaterThanWithOffset offset (Product firstOutputType secondOutputType) outputType
                -- successOutput <- typeGreaterThan (Product firstOutputType secondOutputType) outputType
                -- unless successOutput $ failTypeMatch offset (Product firstOutputType secondOutputType) outputType
                firstArrow' <- assertType (AliasReference (Name (getOffsetFromValue firstArrow) arrowInstance) [onlyInputType, firstOutputType]) firstArrow
                secondArrow' <- assertType (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [onlyInputType, secondOutputType]) secondArrow
                return $ TypeWithTripleAnd arrowType firstArrow' secondArrow'
            (ArrowLeft offset leftArrow) -> do
                bypassedType <- addTypeVariabel
                innerInputType <- addTypeVariabel
                innerOutputType <- addTypeVariabel
                typeGreaterThanWithOffset offset (Sum innerInputType bypassedType) inputType
                -- successInput <- typeGreaterThan (Sum innerInputType bypassedType) inputType
                -- unless successInput $ failTypeMatch offset (Sum innerInputType bypassedType) inputType
                typeGreaterThanWithOffset offset (Sum innerOutputType bypassedType) outputType
                -- successOutput <- typeGreaterThan (Sum innerOutputType bypassedType) outputType
                -- unless successOutput $ failTypeMatch offset (Sum innerOutputType bypassedType) outputType
                leftArrow' <- assertType (AliasReference (Name (getOffsetFromValue leftArrow) arrowInstance) [innerInputType, innerOutputType]) leftArrow
                return $ TypeWithArrowLeft arrowType leftArrow'
            (ArrowRight offset rightArrow) -> do
                bypassedType <- addTypeVariabel
                innerInputType <- addTypeVariabel
                innerOutputType <- addTypeVariabel
                typeGreaterThanWithOffset offset (Sum bypassedType innerInputType) inputType
                -- successInput <- typeGreaterThan (Sum bypassedType innerInputType) inputType
                -- unless successInput $ failTypeMatch offset (Sum bypassedType innerInputType) inputType
                typeGreaterThanWithOffset offset (Sum bypassedType innerOutputType) outputType
                -- successOutput <- typeGreaterThan (Sum bypassedType innerOutputType) outputType
                -- unless successOutput $ failTypeMatch offset (Sum bypassedType innerOutputType) outputType
                rightArrow' <- assertType (AliasReference (Name (getOffsetFromValue rightArrow) arrowInstance) [innerInputType, innerOutputType]) rightArrow
                return $ TypeWithArrowRight arrowType rightArrow'
            (TriplePlus offset leftArrow rightArrow) -> do
                leftInputType <- addTypeVariabel
                leftOutputType <- addTypeVariabel
                rightInputType <- addTypeVariabel
                rightOutputType <- addTypeVariabel
                typeGreaterThanWithOffset offset (Sum leftInputType rightInputType) inputType
                -- successInput <- typeGreaterThan (Sum leftInputType rightInputType) inputType
                -- unless successInput $ failTypeMatch offset (Sum leftInputType rightInputType) inputType
                typeGreaterThanWithOffset offset (Sum leftOutputType rightOutputType) outputType
                -- successOutput <- typeGreaterThan (Sum leftOutputType rightOutputType) outputType
                -- unless successOutput $ failTypeMatch offset (Sum leftOutputType rightOutputType) outputType
                leftArrow' <- assertType (AliasReference (Name (getOffsetFromValue leftArrow) arrowInstance) [leftInputType, leftOutputType]) leftArrow
                rightArrow' <- assertType (AliasReference (Name (getOffsetFromValue rightArrow) arrowInstance) [rightInputType, rightOutputType]) rightArrow
                return $ TypeWithTriplePlus arrowType leftArrow' rightArrow'
            (TripleBar offset leftArrow rightArrow) -> do
                onlyInputType <- addTypeVariabel
                leftOutputType <- addTypeVariabel
                rightOutputType <- addTypeVariabel
                typeGreaterThanWithOffset offset onlyInputType inputType
                -- successInput <- typeGreaterThan onlyInputType inputType
                -- unless successInput $ failTypeMatch offset onlyInputType inputType
                typeGreaterThanWithOffset offset (Sum leftOutputType rightOutputType) outputType
                -- successOutput <- typeGreaterThan (Sum leftOutputType rightOutputType) outputType
                -- unless successOutput $ failTypeMatch offset (Sum leftOutputType rightOutputType) outputType
                leftArrow' <- assertType (AliasReference (Name (getOffsetFromValue leftArrow) arrowInstance) [onlyInputType, leftOutputType]) leftArrow
                rightArrow' <- assertType (AliasReference (Name (getOffsetFromValue rightArrow) arrowInstance) [onlyInputType, rightOutputType]) rightArrow
                return $ TypeWithTripleBar arrowType leftArrow' rightArrow'
            _ -> undefined

failTypeInference :: Type -> Value -> ParserWithDoubleState [Type] Program TypeWithValue
failTypeInference frame content = do
    setOffset (getOffsetFromValue content)
    references <- get
    program <- lift get
    fail $ "could not match " ++ displayReferentialType (ReferentialType frame references) ++ " with " ++ show content ++ "\n\n" ++ show program

failTypeMatch :: Type -> Type -> ParserWithDoubleState [Type] Program ()
failTypeMatch superior inferior = do
    references <- get
    program <- lift get
    fail $ "could not match " ++ displayReferentialType (ReferentialType superior references) ++ " with " ++ displayReferentialType (ReferentialType inferior references) ++ "\n\n" ++ show program

-- setOffset offset

typeGreaterThanWithOffset :: ParsingOffset -> Type -> Type -> ParserWithDoubleState [Type] Program ()
typeGreaterThanWithOffset offset superior inferior = do
    setOffset offset
    typeGreaterThan superior inferior

typeGreaterThan :: Type -> Type -> ParserWithDoubleState [Type] Program ()
typeGreaterThan (TypeReference index) inferior = do
    superior <- gets (!! index)
    typeGreaterThan superior inferior
typeGreaterThan superior (TypeReference index) = do
    inferior <- gets (!! index)
    typeGreaterThan superior inferior
typeGreaterThan superior@(AliasReference superiorName superiorArguments) inferior@(AliasReference inferiorName inferiorArguments) =
    if superiorName == inferiorName
        then zipWithM_ typeGreaterThan superiorArguments inferiorArguments
        else failTypeMatch superior inferior
typeGreaterThan superior@(ForAllInstances requiredClasses) inferior@(AnyType index existingClasses) = do
    if all (`elem` requiredClasses) existingClasses
        then modify $ replace index $ AnyType index requiredClasses
        else failTypeMatch superior inferior
typeGreaterThan superior (AnyType index classNames) = do
    mapM_ (resolveInstanceOf superior) classNames
    modify $ replace index superior
typeGreaterThan (AnyType index classNames) inferior = do
    mapM_ (resolveInstanceOf inferior) classNames
    modify $ replace index inferior
typeGreaterThan (ForAllInstances classNames) inferior = do
    -- inferiorReferentialType <- gets $ ReferentialType inferior
    -- instancesOfTypeNames <- lift $ gets $ map instanceClassName . filter ((== inferiorReferentialType) . instanceType) . instances
    -- unlesmapM_ (isInstanceOf inferior) classNames
    mapM_ (resolveInstanceOf inferior) classNames
typeGreaterThan (Product superiorX superiorY) (Product inferiorX inferiorY) = typeGreaterThan superiorX inferiorX >> typeGreaterThan superiorY inferiorY
typeGreaterThan (Sum superiorX superiorY) (Sum inferiorX inferiorY) = typeGreaterThan superiorX inferiorX >> typeGreaterThan superiorY inferiorY
typeGreaterThan superior inferior = unless (superior == inferior) $ failTypeMatch superior inferior

resolveInstanceOf :: Type -> Name -> ParserWithDoubleState [Type] Program ()
resolveInstanceOf t@(ForAllInstances classNames) className = unless (className `elem` classNames) $ fail $ show t ++ " has no instance of " ++ nameString className
resolveInstanceOf t className = do
    possibleReferentialTypes <- lift $ map instanceType <$> getInstances className
    success <-
        or
            <$> mapM
                ( \possibleReferentialType -> do
                    possibleType <- addReferentialType possibleReferentialType
                    (typeGreaterThan possibleType t $> True) <|> return False
                )
                possibleReferentialTypes
    unless success $ fail $ show t ++ " has no instance of " ++ nameString className

{-
typeGreaterThan :: Type -> Type -> ParserWithDoubleState [Type] Program Bool
typeGreaterThan (TypeReference index) inferior = do
    superior <- gets (!! index)
    typeGreaterThan superior inferior
typeGreaterThan superior (TypeReference index) = do
    inferior <- gets (!! index)
    typeGreaterThan superior inferior
typeGreaterThan (AliasReference superiorName superiorArguments) (AliasReference inferiorName inferiorArguments) =
    if superiorName == inferiorName
        then and <$> zipWithM typeGreaterThan superiorArguments inferiorArguments
        else return False
typeGreaterThan (ForAllInstances requiredClasses) (AnyType index existingClasses) = do
    if all (`elem` requiredClasses) existingClasses
        then modify (replace index $ AnyType index requiredClasses) $> True
        else return False
typeGreaterThan superior (AnyType index classNames) = do
    isInstanceOfAllClasses <- and <$> mapM (isInstanceOf superior) classNames
    if isInstanceOfAllClasses
        then modify (replace index superior) $> True
        else return False
typeGreaterThan (AnyType index classNames) inferior = do
    isInstanceOfAllClasses <- and <$> mapM (isInstanceOf inferior) classNames
    if isInstanceOfAllClasses
        then modify (replace index inferior) $> True
        else return False
typeGreaterThan (ForAllInstances classNames) inferior = do
    inferiorReferentialType <- gets $ ReferentialType inferior
    -- let classNames = map snd classesWithOffsets
    instancesOfTypeNames <- lift $ gets $ map instanceClassName . filter ((== inferiorReferentialType) . instanceType) . instances
    return $ all (`elem` instancesOfTypeNames) classNames
typeGreaterThan (Product superiorX superiorY) (Product inferiorX inferiorY) = (&&) <$> typeGreaterThan superiorX inferiorX <*> typeGreaterThan superiorY inferiorY
typeGreaterThan (Sum superiorX superiorY) (Sum inferiorX inferiorY) = (&&) <$> typeGreaterThan superiorX inferiorX <*> typeGreaterThan superiorY inferiorY
typeGreaterThan superior inferior = return $ superior == inferior

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
-}

addAlias :: [Type] -> ReferentialType -> ParserWithDoubleState [Type] Program Type
addAlias arguments (ReferentialType unincreasedMainType otherTypesWithPlaceholders) = do
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

getTypeFromValue :: Value -> ParserWithDoubleState [Type] Program Type
getTypeFromValue (ProductLiteral _ x y) = Product <$> getTypeFromValue x <*> getTypeFromValue y
getTypeFromValue (SumLiteral _ boolChoice x) = do
    otherType <- addTypeVariabel
    xType <- getTypeFromValue x
    return $ if boolChoice then Sum otherType xType else Sum xType otherType
getTypeFromValue (BoolLiteral offset _) = return $ AliasReference (Name offset "Bool") []
getTypeFromValue (IntLiteral offset _) = return $ AliasReference (Name offset "Int") []
getTypeFromValue (FloatLiteral offset _) = return $ AliasReference (Name offset "Float") []
getTypeFromValue (CharLiteral offset _) = return $ AliasReference (Name offset "Char") []
getTypeFromValue (EmptyTupleLiteral offset) = return $ AliasReference (Name offset "()") []
getTypeFromValue _ = undefined

-- getTypeFromValue (DefinedValue name) = getTypeFromName name
--     | DefinedValueFromInstance Name Int
--     | ArrowComposition ParsingOffset Value Value
--     | ArrowConstant ParsingOffset Value
--     | ArrowFirst ParsingOffset Value
--     | ArrowSecond ParsingOffset Value
--     | TripleAsterisks ParsingOffset Value Value
--     | TripleAnd ParsingOffset Value Value
--     | ArrowRight ParsingOffset Value
--     | ArrowLeft ParsingOffset Value
--     | TriplePlus ParsingOffset Value Value
--     | TripleBar ParsingOffset Value Value
--     | Undefined ParsingOffset
