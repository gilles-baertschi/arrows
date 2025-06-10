module Helpers where

import Ast
import Control.Monad
import Control.Monad.State
import Data.List
import Parser.Primitives

getAlias :: Name -> ParserWithState Program TypeAlias
getAlias name = gets $ head . filter ((name ==) . aliasName) . aliases

-- getDefinition :: String -> ParserWithState Program Definition
-- getDefinition name = gets $ head . filter ((name ==) . definitionName) . definitions

getTypeFromName :: Name -> ParserWithState Program ReferentialType
getTypeFromName name = do
    fromDefinitions <- gets $ map definitionType . filter ((name ==) . definitionName) . definitions
    fromClasses <- gets $ map snd . filter ((name ==) . fst) . typeClassMembers <=< typeClasses
    -- fromInstances <- gets $ filter ((name ==) . fst) . instanceMembers <=< instances
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
        -- map (\(_, referentialType, _) -> referentialType) . concatMap (filter (\(memberName, _, _) -> name == memberName)) <$> (gets (filter ((== className) . instanceClassName) . instances) >>= mapM getReferentialTypeMembers)
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
        -- map (\(_, referentialType, _) -> referentialType) . concatMap (filter (\(memberName, _, _) -> name == memberName)) <$> (gets (filter ((== className) . instanceClassName) . instances) >>= mapM getReferentialTypeMembers)
        Nothing -> return []
    return $ maybe (Right fromInstances) Left fromDefinition

-- fromInstances <- case maybeClass of
--    Just (TypeClass className members) -> do
--         allDefinitionsFromInstances <- gets $ concatMap (\(Instance instanceType' _ members) -> map (instanceType', undefined) members) . filter ((== className) . instanceClassName) . instances
--         return []
--     Nothing -> return []
-- mapM_
--     ( \(insertedType, (name, value)) -> do
--         referentialType <- getTypeFromName name
--         assertReferentialType (insertType referentialType insertedType 0) value
--     )

getInstances :: Name -> ParserWithState Program [Instance]
getInstances name = gets $ filter ((name ==) . instanceClassName) . instances

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
    (UnaryArrowOperator _ offset _) -> offset
    (BinaryArrowOperator _ offset _ _) -> offset
    (Undefined offset) -> offset

-- (ArrowComposition offset _ _) -> offset
-- (ArrowConstant offset _) -> offset
-- (ArrowFirst offset _) -> offset
-- (ArrowSecond offset _) -> offset
-- (TripleAsterisks offset _ _) -> offset
-- (TripleAnd offset _ _) -> offset
-- (ArrowRight offset _) -> offset
-- (ArrowLeft offset _) -> offset
-- (TriplePlus offset _ _) -> offset
-- (TripleBar offset _ _) -> offset

increaseReferences :: ReferentialType -> Int -> ReferentialType
increaseReferences (ReferentialType t references) index = ReferentialType (increase t) (map increase references)
  where
    increase (Product x y) = Product (increase x) (increase y)
    increase (Sum x y) = Sum (increase x) (increase y)
    increase (AliasReference name arguments) = AliasReference name $ map increase arguments
    increase (TypeReference i) = TypeReference (i + index)
    increase (AliasExtention i arguments) = AliasExtention (i + index) $ map increase arguments
    increase (AnyType i arguments) = AnyType (i + index) arguments
    increase x = x

toAnyTypeReferences :: ReferentialType -> ReferentialType
toAnyTypeReferences (ReferentialType t references) = ReferentialType t (zipWith toAnyType [0 ..] references)
  where
    toAnyType index (Product x y) = Product (toAnyType index x) (toAnyType index y)
    toAnyType index (Sum x y) = Sum (toAnyType index x) (toAnyType index y)
    toAnyType index (AliasReference name arguments) = AliasReference name $ map (toAnyType index) arguments
    toAnyType index (AliasExtention offset arguments) = AliasExtention offset $ map (toAnyType index) arguments
    toAnyType index (ForAllInstances classesWithNames) = AnyType index classesWithNames
    toAnyType _ x = x

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
displayReferentialType (ReferentialType t references) = display t
  where
    display :: Type -> String
    display (Product x y) = "(" ++ display x ++ ", " ++ display y ++ ")"
    display (Sum x y) = "(" ++ display x ++ " | " ++ display y ++ ")"
    -- display Bool = "Bool"
    -- display Float = "Float"
    -- display Int = "Int"
    -- display Char = "Char"
    -- display EmptyTuple = "()"
    display (ForAllInstances classNames) = "for all {" ++ intercalate ", " (map nameString classNames) ++ "}"
    display (AnyType _ classNames) = "any {" ++ intercalate ", " (map nameString classNames) ++ "}"
    display (AliasReference name arguments) = unwords (nameString name : map display arguments)
    display (AliasExtention index extendedArguments) = case references !! index of
        (AliasReference name coreArguments) -> display $ AliasReference name $ coreArguments ++ extendedArguments
        _ -> undefined
    display (TypeReference index) = display $ references !! index
    display ThisClass = "this"

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
addTypeToValue t (DefinedValueFromInstance name index) = TypeWithDefinedValueFromInstance t name index
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

-- getTypeFromTypeWithValue (TypeWithArr t _) = t
-- getTypeFromTypeWithValue (TypeWithArrowComposition t _ _) = t
-- getTypeFromTypeWithValue (TypeWithArrowConstant t _) = t
-- getTypeFromTypeWithValue (TypeWithArrowFirst t _) = t
-- getTypeFromTypeWithValue (TypeWithArrowSecond t _) = t
-- getTypeFromTypeWithValue (TypeWithTripleAsterisks t _ _) = t
-- getTypeFromTypeWithValue (TypeWithTripleAnd t _ _) = t
-- getTypeFromTypeWithValue (TypeWithArrowRight t _) = t
-- getTypeFromTypeWithValue (TypeWithArrowLeft t _) = t
-- getTypeFromTypeWithValue (TypeWithTriplePlus t _ _) = t
-- getTypeFromTypeWithValue (TypeWithTripleBar t _ _) = t

-- addTypeToValue t (ArrowComposition offset x y) = TypeWithArrowComposition t offset x y
-- addTypeToValue t (ArrowConstant offset x) = TypeWithArrowConstant t offset x
-- addTypeToValue t (ArrowFirst offset x) = TypeWithArrowFirst t offset x
-- addTypeToValue t (ArrowSecond offset x) = TypeWithArrowSecond t offset x
-- addTypeToValue t (TripleAsterisks offset x y) = TypeWithTripleAsterisks t offset x y
-- addTypeToValue t (TripleAnd offset x y) = TypeWithTripleAnd t offset x y
-- addTypeToValue t (ArrowRight offset x) = TypeWithArrowRight t offset x
-- addTypeToValue t (ArrowLeft offset x) = TypeWithArrowLeft t offset x
-- addTypeToValue t (TriplePlus offset x y) = TypeWithTriplePlus offset x y
-- addTypeToValue t (TripleBar offset x y) = TypeWithTripleBar t offset x y
--
-- ProductLiteral ParsingOffset Value Value
-- \| SumLiteral ParsingOffset Bool Value
-- \| ArrowComposition ParsingOffset Value Value
-- \| ArrowConstant ParsingOffset Value
-- \| ArrowFirst ParsingOffset Value
-- \| ArrowSecond ParsingOffset Value
-- \| TripleAsterisks ParsingOffset Value Value
-- \| TripleAnd ParsingOffset Value Value
-- \| ArrowRight ParsingOffset Value
-- \| ArrowLeft ParsingOffset Value
-- \| TriplePlus ParsingOffset Value Value
-- \| TripleBar ParsingOffset Value Value
