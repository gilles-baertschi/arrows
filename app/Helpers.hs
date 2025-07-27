module Helpers where

import Ast
import Control.Monad
import Control.Monad.State
import Data.List
import Parser.Primitives

getAlias :: Name -> ParserWithState Program TypeAlias
getAlias name = gets $ head . filter ((name ==) . aliasName) . aliases

getTypeFromName :: Name -> ParserWithState Program ReferentialType
getTypeFromName name = do
    fromDefinitions <- gets $ map definitionType . filter ((name ==) . definitionName) . definitions
    fromClasses <- gets $ map snd . filter ((name ==) . fst) . typeClassMembers <=< typeClasses
    return $ head $ fromDefinitions ++ fromClasses

getTypesFromName :: Name -> ParserWithState Program (Either ReferentialType [ReferentialType])
getTypesFromName name = do
    fromDefinition <- gets $ fmap definitionType . find ((name ==) . definitionName) . definitions
    maybeClass <- gets $ find ((name `elem`) . map fst . typeClassMembers) . typeClasses
    fromInstances <- case maybeClass of
        Just (TypeClass className _ _) -> do
            instancesOfClass <- gets $ filter ((== className) . instanceClassName) . instances
            definitionsForName <- concatMap (filter ((name ==) . definitionName)) <$> mapM getDefinitionsFromInstance instancesOfClass
            return $ map definitionType definitionsForName
        Nothing -> return []
    return $ maybe (Right fromInstances) Left fromDefinition

getDefinitionsFromName :: Name -> ParserWithState Program (Either Definition [Definition])
getDefinitionsFromName name = do
    fromDefinition <- gets $ find ((name ==) . definitionName) . definitions
    maybeClass <- gets $ find ((name `elem`) . map fst . typeClassMembers) . typeClasses
    fromInstances <- case maybeClass of
        Just (TypeClass className _ _) -> do
            instancesOfClass <- gets $ filter ((== className) . instanceClassName) . instances
            concatMap (filter ((name ==) . definitionName)) <$> mapM getDefinitionsFromInstance instancesOfClass
        Nothing -> return []
    return $ maybe (Right fromInstances) Left fromDefinition

getInstances :: Name -> ParserWithState Program [Instance]
getInstances name = gets $ filter ((name ==) . instanceClassName) . instances

getIndeciesFromNameAndInstancType :: Name -> ReferentialType -> ParserWithState Program [Int]
getIndeciesFromNameAndInstancType name referentialType = do 
    instancesWithName <- gets $ filter (elem name . map fst . instanceMembers) . instances
    return $ map fst $ filter ((== referentialType) . instanceType . snd) $ zip [0..] instancesWithName

addTypeVariabel :: ParserWithDoubleState [Type] Program Type
addTypeVariabel = do
    newIndex <- gets length
    modify (++ [AnyType newIndex []])
    return $ TypeReference newIndex

getOffsetFromValue :: Value -> ParsingOffset
getOffsetFromValue value = case value of
    (ProductLiteral offset _ _) -> offset
    (SumLiteral offset _ _) -> offset
    (BoolLiteral offset _) -> offset
    (FloatLiteral offset _) -> offset
    (IntLiteral offset _) -> offset
    (CharLiteral offset _) -> offset
    (EmptyTupleLiteral offset) -> offset
    (DefinedValue (Name offset _)) -> offset
    (DefinedValueFromInstance (Name offset _) _) -> offset
    (UnaryArrowOperator _ offset _ _) -> offset
    (BinaryArrowOperator _ offset _ _ _) -> offset
    (Undefined offset) -> offset

increaseReferences :: ReferentialType -> Int -> ReferentialType
increaseReferences (ReferentialType t references) index = ReferentialType (increase t) (map increase references)
  where
    increase (Product x y) = Product (increase x) (increase y)
    increase (Sum x y) = Sum (increase x) (increase y)
    increase (AliasReference name arguments) = AliasReference name $ map increase arguments
    increase (TypeReference i) = TypeReference (i + index)
    increase (AliasExtention i arguments) = AliasExtention (i + index) $ map increase arguments
    increase (AnyType i arguments) = AnyType (i + index) arguments
    increase (ForAllInstances i arguments) = ForAllInstances (i + index) arguments
    increase x = x

toAnyTypeReferences :: ReferentialType -> ReferentialType
toAnyTypeReferences (ReferentialType t references) = ReferentialType (toAnyType t) (map toAnyType references)
  where
    toAnyType (Product x y) = Product (toAnyType x) (toAnyType y)
    toAnyType (Sum x y) = Sum (toAnyType x) (toAnyType y)
    toAnyType (AliasReference name arguments) = AliasReference name $ map toAnyType arguments
    toAnyType (AliasExtention offset arguments) = AliasExtention offset $ map toAnyType arguments
    toAnyType (ForAllInstances index classesWithNames) = AnyType index classesWithNames
    toAnyType x = x

insertType :: ReferentialType -> ReferentialType -> Int -> ReferentialType
insertType (ReferentialType outerMainType outerOtherTypes) nonIncreasedInsertedTypes index = ReferentialType outerMainType $ replace index insertedMainType outerOtherTypes ++ insertedOtherTypes
  where
    (ReferentialType insertedMainType insertedOtherTypes) = increaseReferences nonIncreasedInsertedTypes $ length outerOtherTypes

replace :: Int -> a -> [a] -> [a]
replace i x xs = before ++ [x] ++ after
  where
    (before, rest) = splitAt i xs
    after = case rest of
        [] -> []
        (_ : after') -> after'

displayReferentialType :: ReferentialType -> String
displayReferentialType (ReferentialType t references) = evalState (display t) []
  where
    display :: Type -> State [Int] String 
    display (Product x y) = do
        xString <- display x
        yString <- display y
        return $ "(" ++ xString ++ ", " ++ yString ++ ")"
    display (Sum x y) = do
        xString <- display x
        yString <- display y
        return $ "(" ++ xString ++ " | " ++ yString ++ ")"
    display (ForAllInstances _ classNames) = return $ "for all {" ++ intercalate ", " (map nameString classNames) ++ "}"
    display (AnyType _ classNames) = return $ "any {" ++ intercalate ", " (map nameString classNames) ++ "}"
    display (AliasReference name arguments) = case name of
        "Id" -> do
            inputString <- display (head arguments)
            outputString <- display (arguments !! 1)
            return $ inputString ++ " -> " ++ outputString
        _ -> unwords . (nameString name :) <$> mapM display arguments
    display (AliasExtention index extendedArguments) = case references !! index of
        (AliasReference name coreArguments) -> display $ AliasReference name $ coreArguments ++ extendedArguments
        _ -> undefined
    display (TypeReference index) = case references !! index of
        (ForAllInstances _ _) -> getName index
        (AnyType _ _) -> getName index
        x -> display x
    display ThisClass = return "this"
    getName :: Int -> State [Int] String
    getName indexInReferences = do
        indecies <- get
        let maybeIndex = elemIndex indexInReferences indecies
        ("a" ++) . show <$> case maybeIndex of
            Nothing -> modify (++ [indexInReferences]) >> return (length indecies)
            (Just indexInNames) -> return indexInNames

getDefinitionsFromInstance :: Instance -> ParserWithState Program [Definition]
getDefinitionsFromInstance (Instance insertedType className members) = do
    (TypeClass _ _ classMembers) <- gets $ head . filter ((className ==) . typeClassName) . typeClasses
    mapM
        ( \((name, referentialType), value) -> do
            return $ Definition name (insertType referentialType insertedType 0) value
        )
        $ zip (sortOn fst classMembers) (map snd $ sortOn fst members)

addTypeToValue :: Type -> Value -> TypeWithValue
addTypeToValue t (BoolLiteral _ x) = TypeWithBoolLiteral t x
addTypeToValue t (IntLiteral _ x) = TypeWithIntLiteral t x
addTypeToValue t (FloatLiteral _ x) = TypeWithFloatLiteral t x
addTypeToValue t (CharLiteral _ x) = TypeWithCharLiteral t x
addTypeToValue t (EmptyTupleLiteral _) = TypeWithEmptyTupleLiteral t
addTypeToValue t (DefinedValue name) = TypeWithDefinedValue t name
addTypeToValue t (DefinedValueFromInstance name (Left index)) = TypeWithDefinedValueFromInstance t name index
addTypeToValue t (Undefined _) = TypeWithUndefined t
addTypeToValue t@(Product xType yType) (ProductLiteral _ xValue yValue) = TypeWithProductLiteral t (addTypeToValue xType xValue) (addTypeToValue yType yValue)
addTypeToValue t@(Sum xType yType) (SumLiteral _ boolChoice value) = TypeWithSumLiteral t boolChoice $ if boolChoice then addTypeToValue yType value else addTypeToValue xType value
addTypeToValue _ _ = undefined

getTypeFromTypeWithValue :: TypeWithValue -> Type
getTypeFromTypeWithValue typeWithValue = case typeWithValue of
    (TypeWithProductLiteral t _ _) -> t
    (TypeWithSumLiteral t _ _) -> t
    (TypeWithBoolLiteral t _) -> t
    (TypeWithIntLiteral t _) -> t
    (TypeWithFloatLiteral t _) -> t
    (TypeWithCharLiteral t _) -> t
    (TypeWithEmptyTupleLiteral t) -> t
    (TypeWithDefinedValue t _) -> t
    (TypeWithDefinedValueFromInstance t _ _) -> t
    (TypeWithUndefined t) -> t
    (TypeWithUnaryArrowOperator _ t _) -> t
    (TypeWithBinaryArrowOperator _ t _ _) -> t

idArrowType :: ParsingOffset -> ReferentialType
idArrowType offset = ReferentialType (AliasReference (Name offset "Id") []) []

isIdArrow :: ReferentialType -> Bool
isIdArrow (ReferentialType (AliasReference (Name _ "Id") []) []) = True 
isIdArrow _ = False
