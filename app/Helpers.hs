module Helpers where

import Ast
import Control.Monad
import Control.Monad.State
import Parser.Primitives

getAlias :: String -> ParserWithState Program TypeAlias
getAlias name = gets $ head . filter ((name ==) . aliasName) . aliases

-- getDefinition :: String -> ParserWithState Program Definition
-- getDefinition name = gets $ head . filter ((name ==) . definitionName) . definitions

getTypeFromName :: String -> ParserWithState Program ReferentialType
getTypeFromName name = do
    fromDefinitions <- gets $ map definitionType . filter ((name ==) . definitionName) . definitions
    fromClasses <- gets $ map snd . filter ((name ==) . fst) . typeClassMembers <=< typeClasses
    -- instaceMembersWithName <- gets (filter (instanceMembers) . instances)
    return $ head $ fromDefinitions ++ fromClasses

-- getInstanceMembersWithTypes :: ParserWithState Program [(String, ReferentialType, Value)]
-- getInstanceMembersWithTypes = do
--    instancesWithClasses <- getInstancesWithClasses
--    let result = _a == map (\(typeClass, correspondingInstance) -> zipWith _b (sortOn fst (typeClassMembers typeClass)) (sortOn fst (instanceMembers correspondingInstance))) instancesWithClasses

--   (map (\(typeClass, correspondingInstance) -> map (\(referentialType, name) -> (referentialType, name, head filter ((name ==) . snd)) (instanceMembers correspondingInstance))) (typeClassMembers typeClass)) <$> getInstancesWithClasses

-- getInstancesWithClasses :: ParserWithState Program [(TypeClass, Instance)]
-- getInstancesWithClasses = do
--    allClasses <- gets typeClasses
--    gets $ map (\x -> (head $ filter ((instanceClassName x ==) . typeClassName) allClasses, x)) . instances

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
    (DefinedValue offset _) -> offset
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
    increase (AliasReference offset name arguments) = AliasReference offset name $ map increase arguments
    increase (TypeReference i) = TypeReference (i + index)
    increase (AliasExtention offset arguments) = AliasExtention offset $ map increase arguments
    increase x = x

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
