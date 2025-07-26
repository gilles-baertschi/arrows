module Checker.Types where

import Ast
import Control.Monad
import Control.Monad.State
import Data.Functor
import Helpers
import Parser.Primitives
import Text.Megaparsec hiding (State)
import Data.Either
import Data.Bifunctor

checkTypeSafety :: ParserWithState Program ()
checkTypeSafety = do
    gets definitions >>= mapM_ checkDefinition
    gets instances >>= mapM_ (getDefinitionsFromInstance >=> mapM_ checkDefinition)

checkDefinition :: Definition -> ParserWithState Program (TypeWithValue, [Type])
checkDefinition (Definition _ referentialType value) = do 
    result <- assertReferentialType referentialType value
    case result of 
        [] -> undefined
        [singleResult] -> return singleResult
        multipleResults -> fail $ "ambiguous types\n" ++ show multipleResults

assertReferentialType :: ReferentialType -> Value -> ParserWithState Program [(TypeWithValue, [Type])]
assertReferentialType (ReferentialType frame references) content = evalStateT (assertType frame content) references

assertType :: Type -> Value -> ParserWithDoubleState [Type] Program [(TypeWithValue, [Type])]
assertType frame (Undefined _) = returnTypeWithValueAndState $ TypeWithUndefined frame
assertType (TypeReference index) content = do
    when (index < 0) $ fail $ "tried resolving negatide reference: " ++ show content
    frame <- gets (!! index)
    -- case frame of
    --     (ForAllInstances typeClassNames) -> undefined
    --     _ -> assertType frame content
    assertType frame content
-- assertType frame@(AliasReference name [inputType, outputType]) content@(UnaryArrowOperator {}) = assertArrow (nameString name) inputType outputType content
-- assertType frame@(AliasReference name [inputType, outputType]) content@(BinaryArrowOperator {}) = assertArrow (nameString name) inputType outputType content
assertType frame content@(DefinedValue name) = do
    eitherReferentialTypes <- lift $ getTypesFromName name
    case eitherReferentialTypes of
        (Left fromDefinition) -> do
            inferiorType <- addReferentialType $ toAnyTypeReferences fromDefinition
            typeGreaterThanWithOffset (nameOffset name) frame inferiorType
            returnTypeWithValueAndState $ TypeWithDefinedValue frame name
        (Right fromInstances) -> do
            -- let x = if name == "arr" then [head fromInstances] else fromInstances
            possibleIndecies <-
                concat
                    <$> mapM
                        ( \(index, fromInstance) -> do
                             backup <- get
                             ( do
                                 inferiorType <- addReferentialType $ toAnyTypeReferences fromInstance
                                 typeGreaterThanWithOffset (nameOffset name) frame inferiorType
                                 state <- get
                                 put backup
                                 return [(TypeWithDefinedValueFromInstance frame name index, state)]
                                 ) <|> (put backup >> return [])
                        )
                        (zip [0 ..] fromInstances)
            case possibleIndecies of
                [] -> failTypeInference frame content
                xs -> return xs
                -- [index] -> return $ TypeWithDefinedValueFromInstance frame name index
                -- index:xs -> return $ TypeWithDefinedValueFromInstance frame name index
assertType frame content@(DefinedValueFromInstance name (Left index)) = do
    fromInstance <- lift $ (!! index) . fromRight undefined <$> getTypesFromName name
    inferiorType <- addReferentialType $ toAnyTypeReferences fromInstance
    typeGreaterThanWithOffset (nameOffset name) frame inferiorType
    returnTypeWithValueAndState $ TypeWithDefinedValueFromInstance frame name index
assertType frame content@(DefinedValueFromInstance name (Right instancedType)) = do
    instancesWithName <- lift $ gets $ filter ((name `elem`) . map fst . instanceMembers) . instances
    let possibleInstances = filter ((== instancedType) . instanceType . snd) $ zip [0..] instancesWithName
    case possibleInstances of
        [] -> do 
            setOffset $ nameOffset name
            fail $ "no instance of the type " ++ displayReferentialType instancedType
        [(index, singleInstance)] -> assertType frame $ DefinedValueFromInstance name $ Left index
        _ -> do 
            -- setOffset $ nameOffset name
            fail $ "multiple instances of the type " ++ displayReferentialType instancedType
assertType frame content@(UnaryArrowOperator ArrowConstant offset referentialType innerValue) = if maybe False isIdArrow referentialType
    then assertAtomicArrow frame content offset "Id" 
    else do
        -- backup <- get
        -- assertAtomicArrow frame content offset "Id" <|> assertAtomicArrow frame content offset "IO" <|> put backup >> 
        assertType frame $ UnaryArrowOperator Arr offset Nothing $ UnaryArrowOperator ArrowConstant offset (Just $ idArrowType offset) innerValue
assertType frame content@(UnaryArrowOperator unaryOperator offset referentialType innerArrow) =  if maybe False isIdArrow referentialType
    then assertAtomicArrow frame content offset "Id" 
    else assertArrowOperator frame content offset operator innerArrow
  where
    operator = DefinedValue $ Name offset $ show unaryOperator
assertType frame content@(BinaryArrowOperator binaryOperator offset referentialType firstArrow secondArrow) = if maybe False isIdArrow referentialType
    then assertAtomicArrow frame content offset "Id" 
    else assertArrowOperator frame content offset operator inner
  where
    operator = DefinedValue $ Name offset $ show binaryOperator
    inner = ProductLiteral offset firstArrow secondArrow
    -- let maybeAtomicArrow = case frame of 
    --                                     (AliasReference name arguments) -> if name == "Id" || name == "IO" then Just (name, arguments) else Nothing 
    --                                     _ -> Nothing
    -- case maybeAtomicArrow of
    --     Just (name, [inputType, outputType]) -> assertArrow (nameString name) inputType outputType content
    --     Nothing -> assertType frame $ applyOperator offset (DefinedValue $ Name offset $ show operator) $ ProductLiteral offset firstArrow secondArrow
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
                    (BoolLiteral _ value) -> returnTypeWithValueAndState $ TypeWithBoolLiteral frame value
                    _ -> failTypeInference frame content
                "Float" -> case content of
                    (FloatLiteral _ value) -> returnTypeWithValueAndState $ TypeWithFloatLiteral frame value
                    _ -> failTypeInference frame content
                "Int" -> case content of
                    (IntLiteral _ value) -> returnTypeWithValueAndState $ TypeWithIntLiteral frame value
                    _ -> failTypeInference frame content
                "Char" -> case content of
                    (CharLiteral _ value) -> returnTypeWithValueAndState $ TypeWithCharLiteral frame value
                    _ -> failTypeInference frame content
                "()" -> case content of
                    (EmptyTupleLiteral _) -> returnTypeWithValueAndState $ TypeWithEmptyTupleLiteral frame
                    _ -> failTypeInference frame content
                x -> do 
                    setOffset $ nameOffset name
                    fail $ "tried to resolve undefined type " ++ x 
        (TypeAlias _ _ (Just referentialType)) -> do
            increasedMainType <- addAlias arguments referentialType
            assertType increasedMainType content
assertType (AliasExtention index arguments) content = do
    (AliasReference name arguments') <- gets (!! index)
    assertType (AliasReference name (arguments' ++ arguments)) content
assertType (AnyType index []) content = do
    newType <- getTypeFromValue content
    modify $ replace index newType
    returnTypeWithValueAndState $ addTypeToValue newType content
assertType (AnyType _ _) _ = undefined
assertType (ForAllInstances _ instanceNames) content = fail $ show instanceNames ++ "\n" ++ show content
assertType ThisClass _ = undefined
assertType frame@(Sum x y) content = case content of
    (SumLiteral _ boolChoice value) -> do
        map (first $ TypeWithSumLiteral frame boolChoice) <$> if boolChoice then assertType y value else assertType x value
        -- returnTypeWithValueAndState $ TypeWithSumLiteral frame boolChoice result
    _ -> do 
        failTypeInference frame content
assertType frame@(Product x y) content = case content of
    (ProductLiteral _ x' y') -> do
        possibleResultsX <- assertType x x'
        -- resultY <- assertType y y'
        -- return $ TypeWithProductLiteral frame resultX resultY
        combineAssertion possibleResultsX (TypeWithProductLiteral frame) y y'
        -- concat <$> mapM (\(resultX, state) ->
        --     map (first $ TypeWithProductLiteral frame resultX) 
        --         <$> assertTypeWithState state y y'
        --         ) possibleResultsX
    _ -> do
        failTypeInference frame content


-- f :: String ->  ParserWithDoubleState [Type] Program String
-- f x = return x

assertArrow :: String -> Type -> Type -> Value -> ParserWithDoubleState [Type] Program [(TypeWithValue, [Type])]
assertArrow arrowInstance inputType outputType content =
    let arrowType = AliasReference (Name (getOffsetFromValue content) arrowInstance) [inputType, outputType]
     in case content of
            (UnaryArrowOperator Arr _ _ value) -> do
                possibleInnerArrows <- assertType (AliasReference "Id" [inputType, outputType]) value
                return $ map (first $ TypeWithUnaryArrowOperator Arr arrowType) possibleInnerArrows
                -- return $ TypeWithUnaryArrowOperator Arr arrowType innerArrow
            (UnaryArrowOperator ArrowConstant _ _ value) -> do
                -- value' <- assertType outputType value
                possibleValues <- assertType outputType value
                return $ map (first $ TypeWithUnaryArrowOperator ArrowConstant arrowType) possibleValues
                -- return $ TypeWithUnaryArrowOperator ArrowConstant arrowType value'
            (BinaryArrowOperator ArrowComposition _ _ firstArrow secondArrow) -> do
                sharedType <- addTypeVariabel
                -- firstArrow' <- assertType (AliasReference (Name (getOffsetFromValue firstArrow) arrowInstance) [inputType, sharedType]) firstArrow
                -- secondArrow' <- assertType (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [sharedType, outputType]) secondArrow
                -- return $ TypeWithBinaryArrowOperator ArrowComposition arrowType firstArrow' secondArrow'
                possibleFirstArrows <- assertType (AliasReference (Name (getOffsetFromValue firstArrow) arrowInstance) [inputType, sharedType]) firstArrow
                -- concat <$> mapM (\(firstArrow', state) ->
                --     map (first $ TypeWithBinaryArrowOperator ArrowComposition arrowType firstArrow') 
                --         <$> assertTypeWithState state (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [sharedType, outputType]) secondArrow
                --     ) possibleFirstArrows
                combineAssertion possibleFirstArrows (TypeWithBinaryArrowOperator ArrowComposition arrowType) (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [sharedType, outputType]) secondArrow
            (UnaryArrowOperator ArrowFirst offset _ firstArrow) -> do
                bypassedType <- addTypeVariabel
                innerInputType <- addTypeVariabel
                innerOutputType <- addTypeVariabel
                typeGreaterThanWithOffset offset (Product innerInputType bypassedType) inputType
                typeGreaterThanWithOffset offset (Product innerOutputType bypassedType) outputType
                -- firstArrow' <- assertType (AliasReference (Name (getOffsetFromValue firstArrow) arrowInstance) [innerInputType, innerOutputType]) firstArrow
                -- return $ TypeWithUnaryArrowOperator ArrowFirst arrowType firstArrow'
                possibleFirstArrows <-  assertType (AliasReference (Name (getOffsetFromValue firstArrow) arrowInstance) [innerInputType, innerOutputType]) firstArrow
                return $ map (first $ TypeWithUnaryArrowOperator ArrowFirst arrowType) possibleFirstArrows
            (UnaryArrowOperator ArrowSecond offset _ secondArrow) -> do
                bypassedType <- addTypeVariabel
                innerInputType <- addTypeVariabel
                innerOutputType <- addTypeVariabel
                typeGreaterThanWithOffset offset (Product bypassedType innerInputType) inputType
                typeGreaterThanWithOffset offset (Product bypassedType innerOutputType) outputType
                -- secondArrow' <- assertType (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [innerInputType, innerOutputType]) secondArrow
                -- return $ TypeWithUnaryArrowOperator ArrowSecond arrowType secondArrow'
                possibleSecondArrows <- assertType (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [innerInputType, innerOutputType]) secondArrow
                return $ map (first $ TypeWithUnaryArrowOperator ArrowSecond arrowType) possibleSecondArrows
            (BinaryArrowOperator TripleAsterisks offset _ firstArrow secondArrow) -> do
                firstInputType <- addTypeVariabel
                firstOutputType <- addTypeVariabel
                secondInputType <- addTypeVariabel
                secondOutputType <- addTypeVariabel
                typeGreaterThanWithOffset offset (Product firstInputType secondInputType) inputType
                typeGreaterThanWithOffset offset (Product firstOutputType secondOutputType) outputType
                -- firstArrow' <- assertType (AliasReference (Name (getOffsetFromValue firstArrow) arrowInstance) [firstInputType, firstOutputType]) firstArrow
                -- secondArrow' <- assertType (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [secondInputType, secondOutputType]) secondArrow
                -- return $ TypeWithBinaryArrowOperator TripleAsterisks arrowType firstArrow' secondArrow'
                possibleFirstArrows <- assertType (AliasReference (Name (getOffsetFromValue firstArrow) arrowInstance) [firstInputType, firstOutputType]) firstArrow
                combineAssertion possibleFirstArrows (TypeWithBinaryArrowOperator TripleAsterisks arrowType) (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [secondInputType, secondOutputType]) secondArrow
                -- concat <$> mapM (\(firstArrow', state) ->
                --     map (first $ TypeWithBinaryArrowOperator TripleAsterisks arrowType firstArrow') 
                --         <$> assertTypeWithState state (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [secondInputType, secondOutputType]) secondArrow
                --     ) possibleFirstArrows
            (BinaryArrowOperator TripleAnd offset _ firstArrow secondArrow) -> do
                onlyInputType <- addTypeVariabel
                firstOutputType <- addTypeVariabel
                secondOutputType <- addTypeVariabel
                typeGreaterThanWithOffset offset onlyInputType inputType
                typeGreaterThanWithOffset offset (Product firstOutputType secondOutputType) outputType
                -- firstArrow' <- assertType (AliasReference (Name (getOffsetFromValue firstArrow) arrowInstance) [onlyInputType, firstOutputType]) firstArrow
                -- secondArrow' <- assertType (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [onlyInputType, secondOutputType]) secondArrow
                -- return $ TypeWithBinaryArrowOperator TripleAnd arrowType firstArrow' secondArrow'
                possibleFirstArrows <- assertType (AliasReference (Name (getOffsetFromValue firstArrow) arrowInstance) [onlyInputType, firstOutputType]) firstArrow
                combineAssertion possibleFirstArrows (TypeWithBinaryArrowOperator TripleAnd arrowType) (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [onlyInputType, secondOutputType]) secondArrow
                -- concat <$> mapM (\(firstArrow', state) ->
                --     map (first $ TypeWithBinaryArrowOperator TripleAnd arrowType firstArrow') 
                --         <$> assertTypeWithState state (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [onlyInputType, secondOutputType]) secondArrow
                --     ) possibleFirstArrows
            (UnaryArrowOperator ArrowLeft offset _ leftArrow) -> do
                bypassedType <- addTypeVariabel
                innerInputType <- addTypeVariabel
                innerOutputType <- addTypeVariabel
                typeGreaterThanWithOffset offset (Sum innerInputType bypassedType) inputType
                typeGreaterThanWithOffset offset (Sum innerOutputType bypassedType) outputType
                possibleLeftArrows <- assertType (AliasReference (Name (getOffsetFromValue leftArrow) arrowInstance) [innerInputType, innerOutputType]) leftArrow
                return $ map (first $ TypeWithUnaryArrowOperator ArrowLeft arrowType) possibleLeftArrows
                -- leftArrow' <- assertType (AliasReference (Name (getOffsetFromValue leftArrow) arrowInstance) [innerInputType, innerOutputType]) leftArrow
                -- return $ TypeWithUnaryArrowOperator ArrowLeft arrowType leftArrow'
            (UnaryArrowOperator ArrowRight offset _ rightArrow) -> do
                bypassedType <- addTypeVariabel
                innerInputType <- addTypeVariabel
                innerOutputType <- addTypeVariabel
                typeGreaterThanWithOffset offset (Sum bypassedType innerInputType) inputType
                typeGreaterThanWithOffset offset (Sum bypassedType innerOutputType) outputType
                possibleRightArrows <- assertType (AliasReference (Name (getOffsetFromValue rightArrow) arrowInstance) [innerInputType, innerOutputType]) rightArrow
                return $ map (first $ TypeWithUnaryArrowOperator ArrowRight arrowType) possibleRightArrows
                -- rightArrow' <- assertType (AliasReference (Name (getOffsetFromValue rightArrow) arrowInstance) [innerInputType, innerOutputType]) rightArrow
                -- return $ TypeWithUnaryArrowOperator ArrowRight arrowType rightArrow'
            (BinaryArrowOperator TriplePlus offset _ leftArrow rightArrow) -> do
                leftInputType <- addTypeVariabel
                leftOutputType <- addTypeVariabel
                rightInputType <- addTypeVariabel
                rightOutputType <- addTypeVariabel
                typeGreaterThanWithOffset offset (Sum leftInputType rightInputType) inputType
                typeGreaterThanWithOffset offset (Sum leftOutputType rightOutputType) outputType
                -- leftArrow' <- assertType (AliasReference (Name (getOffsetFromValue leftArrow) arrowInstance) [leftInputType, leftOutputType]) leftArrow
                -- rightArrow' <- assertType (AliasReference (Name (getOffsetFromValue rightArrow) arrowInstance) [rightInputType, rightOutputType]) rightArrow
                -- return $ TypeWithBinaryArrowOperator TriplePlus arrowType leftArrow' rightArrow'
                possibleLeftArrows <- assertType (AliasReference (Name (getOffsetFromValue leftArrow) arrowInstance) [leftInputType, leftOutputType]) leftArrow
                combineAssertion possibleLeftArrows (TypeWithBinaryArrowOperator TriplePlus arrowType) (AliasReference (Name (getOffsetFromValue rightArrow) arrowInstance) [rightInputType, rightOutputType]) rightArrow
                -- concat <$> mapM (\(leftArrow', state) ->
                --     map (first $ TypeWithBinaryArrowOperator TriplePlus arrowType leftArrow') 
                --         <$> assertTypeWithState state (AliasReference (Name (getOffsetFromValue rightArrow) arrowInstance) [rightInputType, rightOutputType]) rightArrow
                --     ) possibleLeftArrows
            (BinaryArrowOperator TripleBar offset _ leftArrow rightArrow) -> do
                leftInputType <- addTypeVariabel
                rightInputType <- addTypeVariabel
                typeGreaterThanWithOffset offset (Sum leftInputType rightInputType) inputType
                -- leftArrow' <- assertType (AliasReference (Name (getOffsetFromValue leftArrow) arrowInstance) [leftInputType, outputType]) leftArrow
                -- rightArrow' <- assertType (AliasReference (Name (getOffsetFromValue rightArrow) arrowInstance) [rightInputType, outputType]) rightArrow
                -- return $ TypeWithBinaryArrowOperator TripleBar arrowType leftArrow' rightArrow'
                possibleLeftArrows <- assertType (AliasReference (Name (getOffsetFromValue leftArrow) arrowInstance) [leftInputType, outputType]) leftArrow
                combineAssertion possibleLeftArrows (TypeWithBinaryArrowOperator TripleBar arrowType) (AliasReference (Name (getOffsetFromValue rightArrow) arrowInstance) [rightInputType, outputType]) rightArrow
                -- concat <$> mapM (\(leftArrow', state) ->
                --     map (first $ TypeWithBinaryArrowOperator TripleBar arrowType leftArrow') 
                --         <$> assertTypeWithState state (AliasReference (Name (getOffsetFromValue rightArrow) arrowInstance) [rightInputType, outputType]) rightArrow
                --     ) possibleLeftArrows
            _ -> failTypeInference arrowType content

returnTypeWithValueAndState :: TypeWithValue -> ParserWithDoubleState [Type] Program [(TypeWithValue, [Type])]
returnTypeWithValueAndState typeWithValue = do
    state <- get
    return [(typeWithValue, state)]

assertTypeWithState :: [Type] -> Type -> Value -> ParserWithDoubleState [Type] Program [(TypeWithValue, [Type])]
assertTypeWithState state frame value = do
    put state
    assertType frame value

combineAssertion :: [(TypeWithValue, [Type])] -> (TypeWithValue -> TypeWithValue -> TypeWithValue) -> Type -> Value -> ParserWithDoubleState [Type] Program [(TypeWithValue, [Type])]
combineAssertion possibilities combine frame content = do
    result <- concat <$> mapM (\(x, state) -> do
            (map (first $ combine x) <$> assertTypeWithState state frame content) <|> return []
        ) possibilities
    case result of
        [] -> failTypeInference frame content
        _ -> return result

failTypeInference :: Type -> Value -> ParserWithDoubleState [Type] Program [(TypeWithValue, [Type])]
failTypeInference frame content = do
    setOffset (getOffsetFromValue content)
    references <- get
    -- program <- lift get
    fail $ "could not match " ++ displayReferentialType (ReferentialType frame references) ++ " with " ++ show content

failTypeMatch :: Type -> Type -> ParserWithDoubleState [Type] Program ()
failTypeMatch superior inferior = do
    references <- get
    -- program <- lift get
    fail $ "could not match " ++ displayReferentialType (ReferentialType superior references) ++ " with " ++ displayReferentialType (ReferentialType inferior references)

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
typeGreaterThan (AliasExtention index arguments) inferior = do
    core <- gets (!! index)
    case core of
        (AliasReference name coreArguments) -> typeGreaterThan (AliasReference name (coreArguments ++ arguments)) inferior
        -- (ForAllInstances []) -> return ()
        -- (ForAllInstances [_]) -> return ()
        -- ThisClass -> return ()
        x -> fail $ "expected alias reference " ++ show x
typeGreaterThan superior (AliasExtention index arguments) = do
    core <- gets (!! index)
    case core of
        (AliasReference name coreArguments) -> typeGreaterThan superior (AliasReference name (coreArguments ++ arguments))
        _ -> undefined
typeGreaterThan superior@(ForAllInstances _ requiredClasses) inferior@(AnyType index existingClasses) = do
    if all (`elem` requiredClasses) existingClasses
        then modify $ replace index $ AnyType index requiredClasses
        else failTypeMatch superior inferior
typeGreaterThan superior (AnyType index classNames) = do
    mapM_ (resolveInstanceOf superior) classNames
    modify $ replace index superior 
typeGreaterThan (AnyType index classNames) inferior = do
    mapM_ (resolveInstanceOf inferior) classNames
    modify $ replace index inferior
typeGreaterThan superior@(ForAllInstances superiorIndex superiorClassNames) inferior@(ForAllInstances inferiorIndex inferiorClassNames) = do
    unless (superiorIndex == inferiorIndex) $ failTypeMatch superior inferior
    -- unless (all (`elem` inferiorClassNames) inferiorClassNames) $ failTypeMatch superior inferior
    -- mapM_ (resolveInstanceOf inferior) classNames
typeGreaterThan (Product superiorX superiorY) (Product inferiorX inferiorY) = typeGreaterThan superiorX inferiorX >> typeGreaterThan superiorY inferiorY
typeGreaterThan (Sum superiorX superiorY) (Sum inferiorX inferiorY) = typeGreaterThan superiorX inferiorX >> typeGreaterThan superiorY inferiorY
typeGreaterThan superior inferior = do
    equalsBySameReference <- case (superior, inferior) of
        (AliasReference superiorName superiorArguments, AliasReference inferiorName inferiorArguments) ->
            if superiorName == inferiorName
                then zipWithM_ typeGreaterThan superiorArguments inferiorArguments $> True
                else typeGreaterThanReducibleAliasReference False inferior superiorName superiorArguments
        (AliasReference name arguments, _) -> typeGreaterThanReducibleAliasReference False inferior name arguments
        (_, AliasReference name arguments) -> typeGreaterThanReducibleAliasReference True superior name arguments
        _ -> return False
    unless (equalsBySameReference || superior == inferior) $ failTypeMatch superior inferior

typeGreaterThanReducibleAliasReference :: Bool -> Type -> Name -> [Type] -> ParserWithDoubleState [Type] Program Bool
typeGreaterThanReducibleAliasReference isSuperior other name arguments = do
    alias <- aliasType <$> lift (getAlias name)
    case alias of
        (Just referentialType) -> do
            increasedMainType <- addAlias arguments referentialType
            if isSuperior
                then typeGreaterThan other increasedMainType
                else typeGreaterThan increasedMainType other
            return True
        Nothing -> return False

resolveInstanceOf :: Type -> Name -> ParserWithDoubleState [Type] Program ()
resolveInstanceOf t@(ForAllInstances _ classNames) className = unless (className `elem` classNames) $ fail $ show t ++ " has no instance of " ++ nameString className
resolveInstanceOf t className = do
    possibleReferentialTypes <- lift $ map instanceType <$> getInstances className
    backup <- get
    success <-
        or
            <$> mapM
                ( \possibleReferentialType -> do
                    possibleType <- addReferentialType possibleReferentialType
                    (typeGreaterThan possibleType t $> True) <|> (put backup >> return False)
                )
                possibleReferentialTypes
    unless success $ fail $ show t ++ " has no instance of " ++ nameString className

addAlias :: [Type] -> ReferentialType -> ParserWithDoubleState [Type] Program Type
addAlias arguments (ReferentialType unincreasedMainType otherTypesWithPlaceholders) = do
    let otherTypesWithoutPlaceholders = drop (length arguments) otherTypesWithPlaceholders
    amount <- gets length
    let (ReferentialType increasedMainType newReferences) = increaseReferences (ReferentialType unincreasedMainType otherTypesWithoutPlaceholders) amount
    modify (++ arguments ++ newReferences)
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
getTypeFromValue (DefinedValue name) = do
    referentialType <- lift $ getTypeFromName name
    addReferentialType $ toAnyTypeReferences referentialType
getTypeFromValue (DefinedValueFromInstance name (Left index)) = do
    referentialType <- either id (!! index) <$> lift (getTypesFromName name)
    addReferentialType $ toAnyTypeReferences referentialType
getTypeFromValue (UnaryArrowOperator Arr offset referentialType inner) = do
    let isId = maybe False isIdArrow referentialType
    innerType <- getTypeFromValue inner
    inputType <- addTypeVariabel
    outputType <- addTypeVariabel
    let innerArrow = AliasReference (Name offset "Id") [inputType, outputType]
    typeGreaterThanWithOffset offset innerArrow innerType 
    index <- gets length
    unless isId $ modify (++ [AnyType index ["Arrow"]])
    return $ if isId then AliasReference (Name offset "Id") [inputType, outputType] else AliasExtention index [inputType, outputType]
getTypeFromValue (BinaryArrowOperator ArrowComposition offset referentialType firstInner secondInner) = do
    let isId = maybe False isIdArrow referentialType
    firstInnerType <- getTypeFromValue firstInner
    secondInnerType <- getTypeFromValue secondInner
    inputType <- addTypeVariabel
    outputType <- addTypeVariabel
    sharedType <- addTypeVariabel
    index <- gets length
    modify (++ [AnyType index ["Arrow"]])
    let firstInnerArrow = AliasExtention index [inputType, sharedType]
    let secondInnerArrow = AliasExtention index [sharedType, outputType]
    typeGreaterThanWithOffset offset firstInnerArrow firstInnerType
    typeGreaterThanWithOffset offset secondInnerArrow secondInnerType
    return $ AliasExtention index [inputType, outputType]
getTypeFromValue x = fail $ show x

applyOperator :: ParsingOffset -> Value -> Value -> Value
applyOperator offset operator inner = idArrow 
  where
    idArrow = BinaryArrowOperator ArrowComposition offset (Just $ idArrowType offset) (BinaryArrowOperator ArrowComposition offset (Just $ idArrowType offset) double first) app
    first = UnaryArrowOperator ArrowFirst offset (Just $ idArrowType offset) $ BinaryArrowOperator ArrowComposition offset (Just $ idArrowType offset) constF operator
    constF = UnaryArrowOperator ArrowConstant offset (Just $ idArrowType offset) inner
    app = DefinedValueFromInstance (Name offset "app") (Right $ idArrowType offset)
    double = DefinedValue $ Name offset "double"
-- arr f = double >>> first (const f >>> arr) >>> app

assertArrowOperator :: Type -> Value -> ParsingOffset -> Value -> Value -> ParserWithDoubleState [Type] Program [(TypeWithValue, [Type])]
assertArrowOperator frame content offset operator inner = do
    backup <- get
    idPossibilities <- assertAtomicArrow frame content offset "Id" <|> return []
    put backup
    ioPossibilities <- assertAtomicArrow frame content offset "IO" <|> return []
    put backup
    specialPossibilities <- assertType frame (applyOperator offset operator inner) <|> return []
    put backup
    let possibilities = idPossibilities ++ ioPossibilities ++ specialPossibilities
    case possibilities of
        [] -> failTypeInference frame content
        xs -> return xs

    -- atomic <- (Just <$> assertAtomicArrow frame content offset "Id") <|> (put backup >> Just <$> assertAtomicArrow frame content offset "IO") <|> (put backup $> Nothing)
    -- case atomic of
    --     (Just x) -> return x
    --     Nothing -> do
    --         -- setOffset offset
    --         -- fail "special arrow"
    --         assertType frame $ applyOperator offset operator inner
    -- possibleIndecies <-
        -- concat
        --     <$> mapM
        --         ( \(index, fromInstance) -> do
        --              backup <- get
        --              ( do
        --                 inferiorType <- addReferentialType $ toAnyTypeReferences fromInstance
        --                 typeGreaterThanWithOffset (nameOffset name) frame inferiorType
        --                 state <- get
        --                 put backup
        --                 return [(TypeWithDefinedValueFromInstance frame name index, state)]
        --                 ) <|> (put backup >> return [])
        --              ) (zip [0 ..] fromInstances)
    
    -- let maybeAtomicArrow = case frame of 
    --                                     (AliasReference name arguments) -> if name == "Id" || name == "IO" then Just (name, arguments) else Nothing 
    --                                     _ -> Nothing
    -- case maybeAtomicArrow of
    --     Just (name, [inputType, outputType]) -> assertArrow (nameString name) inputType outputType content
    --     Nothing -> assertType frame $ applyOperator offset (DefinedValue $ Name offset $ show operator) innerArrow
    
assertAtomicArrow :: Type -> Value -> ParsingOffset -> String -> ParserWithDoubleState [Type] Program [(TypeWithValue, [Type])]
assertAtomicArrow frame content offset atomicType = do
    inputType <- addTypeVariabel
    outputType <- addTypeVariabel
    let arrow = AliasReference (Name offset atomicType) [inputType, outputType]
    typeGreaterThanWithOffset offset frame arrow
    assertArrow atomicType inputType outputType content

