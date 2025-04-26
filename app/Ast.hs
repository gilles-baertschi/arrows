module Ast (ParsingOffset, Program (..), TypeClass (..), Instance (..), Definition (..), TypeAlias (..), Value (..), Type (..), ReferentialType (..), resolveReference, makeTypeReferential) where

import Control.Monad.State
import Data.List

data Program = Program {typeClasses :: [TypeClass], instances :: [Instance], definitions :: [Definition], aliases :: [TypeAlias]}
    deriving (Show, Read, Eq, Ord)

data TypeClass = TypeClass {typeClassName :: String, typeClassMembers :: [(String, ReferentialType)]}
    deriving (Show, Read, Eq, Ord)

data Instance = Instance {instanceType :: ReferentialType, instanceClassName :: String, instanceMembers :: [(String, Value)]}
    deriving (Show, Read, Eq, Ord)

data Definition = Definition {definitionName :: String, definitionType :: ReferentialType, definitionValue :: Value}
    deriving (Show, Read, Eq, Ord)

data TypeAlias = TypeAlias {aliasName :: String, aliasArgumentCount :: Int, aliasType :: ReferentialType}
    deriving (Show, Read, Eq, Ord)

data Value
    = ProductLiteral ParsingOffset Value Value
    | SumLiteral ParsingOffset Value Value
    | BoolLiteral ParsingOffset Bool
    | IntLiteral ParsingOffset Int
    | FloatLiteral ParsingOffset Double
    | CharLiteral ParsingOffset Char
    | EmptyTupleLiteral ParsingOffset
    | DefinedValue ParsingOffset String
    | ArrowComposition ParsingOffset Value Value
    | ArrowConstant ParsingOffset Value
    | ArrowFirst ParsingOffset Value
    | ArrowSecond ParsingOffset Value
    | TripleAsterisks ParsingOffset Value Value
    | TripleAnd ParsingOffset Value Value
    | ArrowRight ParsingOffset Value
    | ArrowLeft ParsingOffset Value
    | TriplePlus ParsingOffset Value Value
    | TripleBar ParsingOffset Value Value
    | CompilerDefined ParsingOffset
    deriving (Read, Eq, Ord, Show)

data ReferentialType = ReferentialType {mainType :: Type, otherTypes :: [Type]}
    deriving (Read, Ord, Show)

resolveReference :: ReferentialType -> Int -> ReferentialType
resolveReference (ReferentialType _ references) index = ReferentialType (references !! index) references

makeTypeReferential :: Type -> ReferentialType
makeTypeReferential t = ReferentialType t []

type ParsingOffset = Int

data Type
    = Product Type Type
    | Sum Type Type
    | Bool
    | Float
    | Int
    | Char
    | EmptyTuple
    | ForAllInstances [(ParsingOffset, String)]
    | AnyType Int [(ParsingOffset, String)]
    | AliasReference ParsingOffset String [Type]
    | AliasExtention Int [Type]
    | TypeReference Int
    deriving (Read, Eq, Ord, Show)

instance Eq ReferentialType where
    (ReferentialType mainTypeX otherTypesX) == (ReferentialType mainTypeY otherTypesY) = evalState (eq mainTypeX mainTypeY) (otherTypesX, otherTypesY)
      where
        eq :: Type -> Type -> State ([Type], [Type]) Bool
        eq (TypeReference index) y = do
            x <- gets $ (!! index) . fst
            eq x y
        eq x (TypeReference index) = do
            y <- gets $ (!! index) . snd
            eq x y
        eq (AliasExtention index arguments) y = do
            x <- gets $ (!! index) . fst
            case x of
                (AliasReference offset nameX argumentsX) -> eq (AliasReference offset nameX (argumentsX ++ arguments)) y
                _ -> return False
        eq x (AliasExtention index arguments) = do
            y <- gets $ (!! index) . fst
            case y of
                (AliasReference offset nameY argumentsY) -> eq x (AliasReference offset nameY (argumentsY ++ arguments))
                _ -> return False
        eq (Product x1 x2) (Product y1 y2) = do
            eq1 <- eq x1 y1
            eq2 <- eq x2 y2
            return $ eq1 && eq2
        eq (Sum x1 x2) (Sum y1 y2) = do
            eq1 <- eq x1 y1
            eq2 <- eq x2 y2
            return $ eq1 && eq2
        eq (ForAllInstances classesWithNamesX) (ForAllInstances classesWithNamesY) = do
            let classesX = sort $ map snd classesWithNamesX
            let classesY = sort $ map snd classesWithNamesY
            return $ classesX == classesY
        eq (AnyType _ classesWithNamesX) (AnyType _ classesWithNamesY) = do
            let classesX = sort $ map snd classesWithNamesX
            let classesY = sort $ map snd classesWithNamesY
            return $ classesX == classesY
        eq (AliasReference _ nameX argumentsX) (AliasReference _ nameY argumentsY) = do
            if nameX == nameY && length argumentsX == length argumentsY
                then and <$> zipWithM eq argumentsX argumentsY
                else return False
        eq x y = return $ x == y
