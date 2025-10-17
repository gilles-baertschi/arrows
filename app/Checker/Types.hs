{-# LANGUAGE LambdaCase #-}

module Checker.Types where

import Ast
import Control.Monad
import Control.Monad.State
import Data.Bifunctor
import Data.Either
import Data.Functor
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Helpers
import Parser.Primitives
import Text.Megaparsec hiding (State)

type TranslationResult = (Maybe TypeWithValue, [Type])

checkTypeSafety :: ParserWithState Program ()
checkTypeSafety = do
  gets instances >>= mapM_ (getDefinitionsFromInstance >=> mapM_ checkDefinition)
  gets definitions >>= mapM_ checkDefinition

-- error $ "123"

checkDefinition :: Definition -> ParserWithState Program TranslationResult
checkDefinition (Definition _ referentialType value) = do
  results <- assertReferentialType referentialType value
  let concreteResults = filter (maybe False isTranslateable . fst) results
  if not (null concreteResults)
    then case concreteResults of
      [] -> undefined
      [singleResult] -> return singleResult
      _ -> fail "ambiguous types\n"
    else case results of
      [] -> undefined
      [singleResult] -> return singleResult
      _ -> fail "ambiguous types\n"

isTranslateable :: TypeWithValue -> Bool
isTranslateable (TypeWithProductLiteral _ x y) = isTranslateable x && isTranslateable y
isTranslateable (TypeWithSumLiteral _ _ x) = isTranslateable x
isTranslateable (TypeWithUnaryArrowOperator _ _ x) = isTranslateable x
isTranslateable (TypeWithBinaryArrowOperator _ _ x y) = isTranslateable x && isTranslateable y
isTranslateable (TypeWithUntranslateable _) = False
isTranslateable _ = True

assertReferentialType :: ReferentialType -> Value -> ParserWithState Program [TranslationResult]
assertReferentialType (ReferentialType frame references) content = evalStateT (assertType frame content) references

assertType :: Type -> Value -> ParserWithDoubleState [Type] Program [TranslationResult]
assertType frame (Undefined _ sourcePos) = returnTypeWithValueAndState $ TypeWithUndefined frame sourcePos
assertType (TypeReference index) content = do
  when (index < 0) $ fail $ "tried resolving negative reference: " ++ show content
  frame <- gets (!! index)
  -- case frame of
  --     (ForAllInstances typeClassNames) -> undefined
  --     _ -> assertType frame content
  assertType frame content
assertType frame content@(DefinedValue name) = do
  eitherReferentialTypes <- lift $ getTypesFromName name
  case eitherReferentialTypes of
    (Left fromDefinition) -> do
      inferiorType <- addReferentialType $ toAnyTypeReferences fromDefinition Nothing
      typeGreaterThanWithOffset (nameOffset name) frame inferiorType
      returnTypeWithValueAndState $ TypeWithDefinedValue frame name
    -- error $ show result
    (Right fromInstances) -> do
      backup <- get
      possibleIndecies <-
        concat
          <$> mapM
            ( \(index, fromInstance) ->
                observingFail
                  ( do
                      put backup
                      thisType <- lift $ gets $ instanceType . (!! index) . filter (elem name . map fst . instanceMembers) . instances
                      inferiorType <- addReferentialType $ toAnyTypeReferences fromInstance $ Just thisType
                      -- x <- get
                      -- error $ show (frame, x)
                      typeGreaterThanWithOffset (nameOffset name) frame inferiorType
                      s <- get
                      return [(Just $ TypeWithDefinedValueFromInstance frame name index, s)]
                  )
            )
            (zip [0 ..] fromInstances)
      put backup
      general <-
        observingFail
          ( do
              preGeneralType <- lift $ getTypeFromName name
              generalClassName <- lift $ gets $ typeClassName . head . filter (elem name . map fst . typeClassMembers) . typeClasses
              let generalThisClass = ReferentialType (TypeReference 0) [AnyType 0 [generalClassName]]
              generalType <- addReferentialType $ specifieClass preGeneralType generalThisClass
              typeGreaterThanWithOffset (nameOffset name) frame generalType
              s <- get
              -- when (null $ rights possibleIndecies) $ error $ show (generalType, s)
              return [(Just $ TypeWithUntranslateable generalType, s)]
          )
      put backup
      case partitionEithers (possibleIndecies ++ general) of
        ([], []) -> failTypeInference frame content
        (failures, []) -> failMultiple failures -- failTypeInference frame content
        (_, xs) -> return xs
assertType frame (DefinedValueFromInstance name (Left index)) = do
  fromInstance <- lift $ (!! index) . fromRight undefined <$> getTypesFromName name
  thisType <- lift $ gets $ instanceType . (!! index) . filter (elem name . map fst . instanceMembers) . instances
  inferiorType <- addReferentialType $ toAnyTypeReferences fromInstance $ Just thisType
  typeGreaterThanWithOffset (nameOffset name) frame inferiorType
  returnTypeWithValueAndState $ TypeWithDefinedValueFromInstance frame name index
assertType frame (DefinedValueFromInstance name (Right instancedType)) = do
  possibleIndecies <- lift $ getIndeciesFromNameAndInstanceType name instancedType
  case possibleIndecies of
    [] -> do
      setOffset $ nameOffset name
      fail $ "no instance of the type " ++ displayReferentialType instancedType
    [index] -> assertType frame $ DefinedValueFromInstance name $ Left index
    _ -> do
      setOffset $ nameOffset name
      fail $ "multiple instances of the type " ++ displayReferentialType instancedType
assertType frame content@(UnaryArrowOperator ArrowConstant offset referentialType innerValue) =
  if maybe False isIdArrow referentialType
    then assertAtomicArrow frame content offset "Id"
    else assertType frame $ UnaryArrowOperator Arr offset Nothing $ UnaryArrowOperator ArrowConstant offset (Just $ idArrowType offset) innerValue
assertType frame content@(UnaryArrowOperator unaryOperator offset referentialType innerArrow) =
  if maybe False isIdArrow referentialType
    then assertAtomicArrow frame content offset "Id"
    else assertArrowOperator frame content offset (show unaryOperator) referentialType innerArrow
assertType frame content@(BinaryArrowOperator binaryOperator offset referentialType firstArrow secondArrow) =
  if maybe False isIdArrow referentialType
    then assertAtomicArrow frame content offset "Id"
    else assertArrowOperator frame content offset (show binaryOperator) referentialType (ProductLiteral offset firstArrow secondArrow)
assertType frame@(AliasReference name arguments) content = do
  alias <- lift $ getAlias name
  case alias of
    (TypeAlias _ _ Nothing) ->
      case nameString name of
        -- "Id" ->
        --     case arguments of
        --         [inputType, outputType] -> assertArrow "Id" inputType outputType content
        --         _ -> undefined
        -- "IO" ->
        --     case arguments of
        --         [inputType, outputType] -> assertArrow "IO" inputType outputType content
        --         _ -> undefined
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
  newTypes <- getTypeFromValue content
  concat
    <$> mapM
      ( \newType -> do
          modify $ replace index newType
          returnTypeWithValueAndState $ addTypeToValue newType content
      )
      newTypes
assertType (AnyType _ _) _ = undefined
assertType (ForAllInstances _ instanceNames) content = fail $ show instanceNames ++ "\n" ++ show content
assertType ThisClass _ = undefined
assertType frame@(Sum x y) content = case content of
  (SumLiteral _ boolChoice value) -> do
    map (first $ fmap $ TypeWithSumLiteral frame boolChoice) <$> if boolChoice then assertType y value else assertType x value
  _ -> do
    failTypeInference frame content
assertType frame@(Product x y) content = case content of
  (ProductLiteral _ x' y') -> do
    possibleResultsX <- assertType x x'
    combineAssertion possibleResultsX (TypeWithProductLiteral frame) y y'
  _ -> do
    failTypeInference frame content

assertArrow :: String -> Type -> Type -> Value -> ParserWithDoubleState [Type] Program [TranslationResult]
assertArrow arrowInstance inputType outputType content =
  let arrowType = AliasReference (Name (getOffsetFromValue content) arrowInstance) [inputType, outputType]
   in case content of
        (UnaryArrowOperator Arr _ _ value) -> do
          possibleInnerArrows <- assertType (AliasReference "Id" [inputType, outputType]) value
          return $ map (first $ fmap $ TypeWithUnaryArrowOperator Arr arrowType) possibleInnerArrows
        (UnaryArrowOperator ArrowConstant _ _ value) -> do
          possibleValues <- assertType outputType value
          return $ map (first $ fmap $ TypeWithUnaryArrowOperator ArrowConstant arrowType) possibleValues
        (BinaryArrowOperator ArrowComposition _ _ firstArrow secondArrow) -> do
          sharedType <- addTypeVariabel
          possibleFirstArrows <- assertType (AliasReference (Name (getOffsetFromValue firstArrow) arrowInstance) [inputType, sharedType]) firstArrow
          -- error $ show (possibleFirstArrows)
          combineAssertion possibleFirstArrows (TypeWithBinaryArrowOperator ArrowComposition arrowType) (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [sharedType, outputType]) secondArrow
        (UnaryArrowOperator ArrowFirst offset _ firstArrow) -> do
          bypassedType <- addTypeVariabel
          innerInputType <- addTypeVariabel
          innerOutputType <- addTypeVariabel
          typeGreaterThanWithOffset offset (Product innerInputType bypassedType) inputType
          typeGreaterThanWithOffset offset (Product innerOutputType bypassedType) outputType
          possibleFirstArrows <- assertType (AliasReference (Name (getOffsetFromValue firstArrow) arrowInstance) [innerInputType, innerOutputType]) firstArrow
          return $ map (first $ fmap $ TypeWithUnaryArrowOperator ArrowFirst arrowType) possibleFirstArrows
        (UnaryArrowOperator ArrowSecond offset _ secondArrow) -> do
          bypassedType <- addTypeVariabel
          innerInputType <- addTypeVariabel
          innerOutputType <- addTypeVariabel
          typeGreaterThanWithOffset offset (Product bypassedType innerInputType) inputType
          typeGreaterThanWithOffset offset (Product bypassedType innerOutputType) outputType
          possibleSecondArrows <- assertType (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [innerInputType, innerOutputType]) secondArrow
          return $ map (first $ fmap $ TypeWithUnaryArrowOperator ArrowSecond arrowType) possibleSecondArrows
        (BinaryArrowOperator TripleAsterisks offset _ firstArrow secondArrow) -> do
          firstInputType <- addTypeVariabel
          firstOutputType <- addTypeVariabel
          secondInputType <- addTypeVariabel
          secondOutputType <- addTypeVariabel
          typeGreaterThanWithOffset offset (Product firstInputType secondInputType) inputType
          typeGreaterThanWithOffset offset (Product firstOutputType secondOutputType) outputType
          possibleFirstArrows <- assertType (AliasReference (Name (getOffsetFromValue firstArrow) arrowInstance) [firstInputType, firstOutputType]) firstArrow
          combineAssertion possibleFirstArrows (TypeWithBinaryArrowOperator TripleAsterisks arrowType) (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [secondInputType, secondOutputType]) secondArrow
        (BinaryArrowOperator TripleAnd offset _ firstArrow secondArrow) -> do
          onlyInputType <- addTypeVariabel
          firstOutputType <- addTypeVariabel
          secondOutputType <- addTypeVariabel
          typeGreaterThanWithOffset offset onlyInputType inputType
          typeGreaterThanWithOffset offset (Product firstOutputType secondOutputType) outputType
          possibleFirstArrows <- assertType (AliasReference (Name (getOffsetFromValue firstArrow) arrowInstance) [onlyInputType, firstOutputType]) firstArrow
          combineAssertion possibleFirstArrows (TypeWithBinaryArrowOperator TripleAnd arrowType) (AliasReference (Name (getOffsetFromValue secondArrow) arrowInstance) [onlyInputType, secondOutputType]) secondArrow
        (UnaryArrowOperator ArrowLeft offset _ leftArrow) -> do
          bypassedType <- addTypeVariabel
          innerInputType <- addTypeVariabel
          innerOutputType <- addTypeVariabel
          typeGreaterThanWithOffset offset (Sum innerInputType bypassedType) inputType
          typeGreaterThanWithOffset offset (Sum innerOutputType bypassedType) outputType
          possibleLeftArrows <- assertType (AliasReference (Name (getOffsetFromValue leftArrow) arrowInstance) [innerInputType, innerOutputType]) leftArrow
          return $ map (first $ fmap $ TypeWithUnaryArrowOperator ArrowLeft arrowType) possibleLeftArrows
        (UnaryArrowOperator ArrowRight offset _ rightArrow) -> do
          bypassedType <- addTypeVariabel
          innerInputType <- addTypeVariabel
          innerOutputType <- addTypeVariabel
          typeGreaterThanWithOffset offset (Sum bypassedType innerInputType) inputType
          typeGreaterThanWithOffset offset (Sum bypassedType innerOutputType) outputType
          possibleRightArrows <- assertType (AliasReference (Name (getOffsetFromValue rightArrow) arrowInstance) [innerInputType, innerOutputType]) rightArrow
          return $ map (first $ fmap $ TypeWithUnaryArrowOperator ArrowRight arrowType) possibleRightArrows
        (BinaryArrowOperator TriplePlus offset _ leftArrow rightArrow) -> do
          leftInputType <- addTypeVariabel
          leftOutputType <- addTypeVariabel
          rightInputType <- addTypeVariabel
          rightOutputType <- addTypeVariabel
          typeGreaterThanWithOffset offset (Sum leftInputType rightInputType) inputType
          typeGreaterThanWithOffset offset (Sum leftOutputType rightOutputType) outputType
          possibleLeftArrows <- assertType (AliasReference (Name (getOffsetFromValue leftArrow) arrowInstance) [leftInputType, leftOutputType]) leftArrow
          combineAssertion possibleLeftArrows (TypeWithBinaryArrowOperator TriplePlus arrowType) (AliasReference (Name (getOffsetFromValue rightArrow) arrowInstance) [rightInputType, rightOutputType]) rightArrow
        (BinaryArrowOperator TripleBar offset _ leftArrow rightArrow) -> do
          leftInputType <- addTypeVariabel
          rightInputType <- addTypeVariabel
          typeGreaterThanWithOffset offset (Sum leftInputType rightInputType) inputType
          -- s <- get
          -- error $ show (leftInputType, leftArrow, s)
          possibleLeftArrows <- assertType (AliasReference (Name (getOffsetFromValue leftArrow) arrowInstance) [leftInputType, outputType]) leftArrow
          combineAssertion possibleLeftArrows (TypeWithBinaryArrowOperator TripleBar arrowType) (AliasReference (Name (getOffsetFromValue rightArrow) arrowInstance) [rightInputType, outputType]) rightArrow
        _ -> failTypeInference arrowType content

returnTypeWithValueAndState :: TypeWithValue -> ParserWithDoubleState [Type] Program [TranslationResult]
returnTypeWithValueAndState typeWithValue = do
  s <- get
  return [(Just typeWithValue, s)]

assertTypeWithState :: [Type] -> Type -> Value -> ParserWithDoubleState [Type] Program [TranslationResult]
assertTypeWithState s frame value = do
  put s
  assertType frame value

combineAssertion :: [TranslationResult] -> (TypeWithValue -> TypeWithValue -> TypeWithValue) -> Type -> Value -> ParserWithDoubleState [Type] Program [TranslationResult]
combineAssertion possibilities combine frame content = do
  (failures, results) <- partitionEithers . concat <$> mapM (\(x, s) -> observingFail (map (first $ c x) <$> assertTypeWithState s frame content)) possibilities
  case (failures, results) of
    (_, []) -> failMultiple failures -- failTypeInference frame content
    _ -> return results
  where
    c Nothing _ = Nothing
    c _ Nothing = Nothing
    c (Just x) (Just y) = Just $ combine x y

observingFail :: ParserWithDoubleState [Type] Program [TranslationResult] -> ParserWithDoubleState [Type] Program [Either String TranslationResult]
observingFail p = do
  r <- observing p
  return $ case r of
    (Left (FancyError _ xs)) ->
      map
        ( \case
            (ErrorFail message) -> Left message
            _ -> undefined
        )
        $ Set.toList xs
    (Right result) -> map Right result
    _ -> undefined

failTypeInference :: Type -> Value -> ParserWithDoubleState [Type] Program [TranslationResult]
failTypeInference frame content = do
  setOffset (getOffsetFromValue content)
  references <- get
  -- program <- lift get
  fail $ "could not match or infere " ++ displayReferentialType (ReferentialType frame references) ++ " with " ++ displayValue content

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
typeGreaterThan superior@(AliasExtention index arguments) inferior = do
  core <- gets (!! index)
  trueCore <- deepResolveReference core
  case trueCore of
    (AliasReference name coreArguments) -> typeGreaterThan (AliasReference name (coreArguments ++ arguments)) inferior
    -- (AnyType classNames) -> do
    -- mapM_ (resolveInstanceOf inferior) classNames
    (AnyType _ _) -> failTypeMatch superior inferior
    (ForAllInstances _ _) -> failTypeMatch superior inferior
    _ -> undefined
typeGreaterThan superior inferior@(AliasExtention index arguments) = do
  core <- gets (!! index)
  trueCore <- deepResolveReference core
  case trueCore of
    (AliasReference name coreArguments) -> typeGreaterThan superior (AliasReference name (coreArguments ++ arguments))
    (ForAllInstances _ _) -> failTypeMatch superior inferior
    (AnyType _ _) -> failTypeMatch superior inferior
    _ -> error $ show (superior, inferior, trueCore)
typeGreaterThan superior@(ForAllInstances otherIndex requiredClasses) inferior@(AnyType index existingClasses) = do
  if all (`elem` requiredClasses) existingClasses
    then modify $ replace index $ TypeReference otherIndex
    else failTypeMatch superior inferior
typeGreaterThan inferior@(AnyType index existingClasses) superior@(ForAllInstances otherIndex requiredClasses) = do
  if all (`elem` requiredClasses) existingClasses
    then modify $ replace index $ TypeReference otherIndex
    else failTypeMatch superior inferior
typeGreaterThan (AnyType inferiorIndex existingClasses) (AnyType superiorIndex additionalClasses) = do
  modify $ replace superiorIndex $ TypeReference inferiorIndex
  modify $ replace inferiorIndex $ AnyType inferiorIndex $ nub $ existingClasses ++ additionalClasses
-- typeGreaterThan superior@(ForAllInstances superiorIndex _) inferior@(ForAllInstances inferiorIndex _) = do
--     unless (superiorIndex == inferiorIndex) $ failTypeMatch superior inferior
-- unless (all (`elem` inferiorClassNames) inferiorClassNames) $ failTypeMatch superior inferior
-- mapM_ (resolveInstanceOf inferior) classNames
typeGreaterThan (Product superiorX superiorY) (Product inferiorX inferiorY) = typeGreaterThan superiorX inferiorX >> typeGreaterThan superiorY inferiorY
typeGreaterThan (Sum superiorX superiorY) (Sum inferiorX inferiorY) = typeGreaterThan superiorX inferiorX >> typeGreaterThan superiorY inferiorY
typeGreaterThan superior (AnyType index classNames) = do
  -- unless (null classNames) $ error $ "resolving as inferior " ++ show classNames ++ " " ++ show superior
  -- unless (null classNames) $ error $ show (superior, classNames)
  mapM_ (resolveInstanceOf superior) classNames
  modify $ replace index superior
typeGreaterThan (AnyType index classNames) inferior = do
  unless (null classNames) $ fail $ "resolving as superior " ++ show classNames ++ " " ++ show inferior
  -- unless (null classNames) $ error $ show (inferior, classNames)
  mapM_ (resolveInstanceOf inferior) classNames
  modify $ replace index inferior
typeGreaterThan superior inferior = do
  equalsBySameReference <- case (superior, inferior) of
    (AliasReference superiorName superiorArguments, AliasReference inferiorName inferiorArguments) ->
      if superiorName == inferiorName
        then zipWithM_ typeGreaterThan superiorArguments inferiorArguments $> True
        else
          (||)
            <$> typeGreaterThanReducibleAliasReference False inferior superiorName superiorArguments
            <*> typeGreaterThanReducibleAliasReference True superior inferiorName inferiorArguments
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
resolveInstanceOf t@(ForAllInstances _ classNames) className = do
  references <- get
  unless (className `elem` classNames) $ fail $ displayReferentialType (ReferentialType t references) ++ " has no instance of " ++ nameString className
resolveInstanceOf t className = do
  possibleReferentialTypes <- lift $ map instanceType <$> getInstances className
  backup <- get
  r <-
    catMaybes
      <$> mapM
        ( \possibleReferentialType -> do
            put backup
            possibleType <- addReferentialType $ toAnyTypeReferences possibleReferentialType Nothing
            s <- get
            (typeGreaterThan t possibleType $> Just s) <|> return Nothing
        )
        possibleReferentialTypes
  put backup
  references <- get
  -- pos <- getSourcePos
  -- case t of
  --     (Sum _ _) -> error $ show (t, r)
  --     _ -> return ()
  case r of
    [] -> fail $ displayReferentialType (ReferentialType t references) ++ " has no instance of " ++ nameString className
    success : _ -> put success

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

getTypeFromValue :: Value -> ParserWithDoubleState [Type] Program [Type]
getTypeFromValue (ProductLiteral _ x y) = liftM2 Product <$> getTypeFromValue x <*> getTypeFromValue y
getTypeFromValue (SumLiteral _ boolChoice x) = do
  otherType <- addTypeVariabel
  xTypes <- getTypeFromValue x
  return $ if boolChoice then map (Sum otherType) xTypes else map (`Sum` otherType) xTypes
getTypeFromValue (BoolLiteral offset _) = return [AliasReference (Name offset "Bool") []]
getTypeFromValue (IntLiteral offset _) = return [AliasReference (Name offset "Int") []]
getTypeFromValue (FloatLiteral offset _) = return [AliasReference (Name offset "Float") []]
getTypeFromValue (CharLiteral offset _) = return [AliasReference (Name offset "Char") []]
getTypeFromValue (EmptyTupleLiteral offset) = return [AliasReference (Name offset "()") []]
getTypeFromValue (DefinedValue name) = do
  -- referentialType <- lift $ getTypeFromName name
  -- maybeClass <- lift $ gets $ find ((name `elem`) . map fst . typeClassMembers) . typeClasses
  -- addReferentialType $ toAnyTypeReferences referentialType $ flip ReferentialType [] . ForAllInstances 0 . (\x -> typeClassName x : typeClassParents x) <$> maybeClass
  eitherType <- lift $ getTypesFromName name
  case eitherType of
    Left x -> (: []) <$> addReferentialType (toAnyTypeReferences x Nothing)
    Right xs -> mapM (\x -> addReferentialType $ toAnyTypeReferences x Nothing) xs
getTypeFromValue (DefinedValueFromInstance name (Left index)) = do
  referentialType <- either id (!! index) <$> lift (getTypesFromName name)
  thisType <- lift $ gets $ instanceType . (!! index) . filter (elem name . map fst . instanceMembers) . instances
  (: []) <$> addReferentialType (toAnyTypeReferences referentialType $ Just thisType)
getTypeFromValue (DefinedValueFromInstance name (Right instancedType)) = do
  possibleIndecies <- lift $ getIndeciesFromNameAndInstanceType name instancedType
  case possibleIndecies of
    [] -> do
      setOffset $ nameOffset name
      fail $ "no instance of the type " ++ displayReferentialType instancedType
    [index] -> getTypeFromValue $ DefinedValueFromInstance name $ Left index
    _ -> do
      setOffset $ nameOffset name
      fail $ "multiple instances of the type " ++ displayReferentialType instancedType

-- getTypeFromValue (UnaryArrowOperator Arr offset referentialType inner) = do
--     let isId = maybe False isIdArrow referentialType
--     innerType <- getTypeFromValue inner
--     inputType <- addTypeVariabel
--     outputType <- addTypeVariabel
--     let innerArrow = AliasReference (Name offset "Id") [inputType, outputType]
--     typeGreaterThanWithOffset offset innerArrow innerType
--     index <- gets length
--     unless isId $ modify (++ [AnyType index ["Arrow"]])
--     return $ if isId then [AliasReference (Name offset "Id") [inputType, outputType]] else [AliasExtention index [inputType, outputType]]
-- getTypeFromValue (BinaryArrowOperator ArrowComposition offset referentialType firstInner secondInner) = do
--     let isId = maybe False isIdArrow referentialType
--     firstInnerType <- getTypeFromValue firstInner
--     secondInnerType <- getTypeFromValue secondInner
--     inputType <- addTypeVariabel
--     outputType <- addTypeVariabel
--     sharedType <- addTypeVariabel
--     index <- gets length
--     modify (++ [AnyType index ["Arrow"]])
--     let firstInnerArrow = AliasExtention index [inputType, sharedType]
--     let secondInnerArrow = AliasExtention index [sharedType, outputType]
--     typeGreaterThanWithOffset offset firstInnerArrow firstInnerType
--     typeGreaterThanWithOffset offset secondInnerArrow secondInnerType
--     return [AliasExtention index [inputType, outputType]]
getTypeFromValue x = fail $ "not implemented getting a type from this value " ++ displayValue x

-- arr f = double >>> first (const f >>> arr) >>> app
applyOperator :: ParsingOffset -> Value -> Value -> Value
applyOperator offset operator inner = BinaryArrowOperator ArrowComposition offset (Just $ idArrowType offset) (BinaryArrowOperator ArrowComposition offset (Just $ idArrowType offset) arrowDouble arrowFirst) arrowApp
  where
    arrowFirst = UnaryArrowOperator ArrowFirst offset (Just $ idArrowType offset) $ BinaryArrowOperator ArrowComposition offset (Just $ idArrowType offset) constF operator
    constF = UnaryArrowOperator ArrowConstant offset (Just $ idArrowType offset) inner
    arrowApp = DefinedValueFromInstance (Name offset "app") (Right $ idArrowType offset)
    arrowDouble = DefinedValue $ Name offset "double"

assertArrowOperator :: Type -> Value -> ParsingOffset -> String -> Maybe ReferentialType -> Value -> ParserWithDoubleState [Type] Program [TranslationResult]
assertArrowOperator frame content offset operatorString referentialType inner = do
  backup <- get
  possibleTypes <- lift $ gets $ filter (\t -> maybe True (== t) referentialType) . map instanceType . filter ((== "Arrow") . instanceClassName) . instances
  -- test <- lift $ gets $ map instanceType . filter ((== "Arrow") . instanceClassName) . instances
  -- fail $ show referentialType
  -- when (null possibleTypes) undefined
  (failures, possibilities) <- partitionEithers . concat <$> mapM (\possibleType -> put backup >> observingFail (f possibleType)) possibleTypes
  case (failures, possibilities) of
    ([], []) -> failTypeInference frame content
    (_, []) -> failMultiple failures -- failTypeInference frame content
    _ -> return possibilities
  where
    f possibleType
      | possibleType == idArrowType offset = assertAtomicArrow frame content offset "Id"
      | possibleType == ReferentialType (AliasReference "IO" []) [] = assertAtomicArrow frame content offset "IO"
      | otherwise = assertType frame (applyOperator offset operator inner)
      where
        operator = DefinedValueFromInstance (Name offset operatorString) $ Right possibleType

failMultiple :: [String] -> ParserWithDoubleState [Type] Program [TranslationResult]
failMultiple failures = do
  fail $ intercalate " or\n" failures

-- backup <- get
-- idPossibilities <- assertAtomicArrow frame content offset "Id" <|> return []
-- put backup
-- ioPossibilities <- assertAtomicArrow frame content offset "IO" <|> return []
-- put backup
-- possibleArrowInstances <- lift $ gets $ filter ((\t -> t /= idArrowType offset && t /= ReferentialType (AliasReference "IO" []) []) . instanceType) . filter ((== "Arrow") . instanceClassName) . instances
-- let operators = map (DefinedValueFromInstance (Name offset operatorString) . Right . instanceType) possibleArrowInstances
-- specialPossibilities <- concat <$> mapM (\operator -> assertType frame (applyOperator offset operator inner) <|> return []) operators
-- put backup
-- let possibilities = idPossibilities ++ ioPossibilities ++ specialPossibilities

assertAtomicArrow :: Type -> Value -> ParsingOffset -> String -> ParserWithDoubleState [Type] Program [TranslationResult]
assertAtomicArrow frame content offset atomicType = do
  inputType <- addTypeVariabel
  outputType <- addTypeVariabel
  let arrow = AliasReference (Name offset atomicType) [inputType, outputType]
  typeGreaterThanWithOffset offset frame arrow
  assertArrow atomicType inputType outputType content

deepResolveReference :: Type -> ParserWithDoubleState [Type] Program Type
deepResolveReference (TypeReference index) = gets (!! index) >>= deepResolveReference
deepResolveReference x = return x
