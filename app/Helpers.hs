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
    return $ head $ fromDefinitions ++ fromClasses

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
    (ArrowComposition offset _ _) -> offset
    (ArrowConstant offset _) -> offset
    (ArrowFirst offset _) -> offset
    (ArrowSecond offset _) -> offset
    (TripleAsterisks offset _ _) -> offset
    (TripleAnd offset _ _) -> offset
    (ArrowRight offset _) -> offset
    (ArrowLeft offset _) -> offset
    (TriplePlus offset _ _) -> offset
    (TripleBar offset _ _) -> offset
    (CompilerDefined offset) -> offset

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
    display Bool = "Bool"
    display Float = "Float"
    display Int = "Int"
    display Char = "Char"
    display EmptyTuple = "()"
    display (ForAllInstances classNames) = "[" ++ intercalate ", " (map nameString classNames) ++ "]"
    display (AnyType _ classNames) = "[" ++ intercalate " | " (map nameString classNames) ++ "]"
    display (AliasReference name arguments) = nameString name ++ " " ++ unwords (map display arguments)
    display (AliasExtention index extendedArguments) = case references !! index of
        (AliasReference name coreArguments) -> display $ AliasReference name $ coreArguments ++ extendedArguments
        _ -> undefined
    display (TypeReference index) = display $ references !! index
