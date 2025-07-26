module Ast {-(
    ParsingOffset,
    Name (..),
    Program (..),
    TypeClass (..),
    Instance (..),
    Definition (..),
    TypeAlias (..),
    Value (..),
    TypeWithValue (..),
    Type (..),
    ReferentialType (..),
    resolveReference,
    makeTypeReferential,
)-} where

import Control.Monad.State
import Data.List
import Data.String

data Program = Program {typeClasses :: [TypeClass], instances :: [Instance], definitions :: [Definition], aliases :: [TypeAlias]}
    deriving (Show, Eq, Ord)

data TypeClass = TypeClass {typeClassName :: Name, typeClassParents :: [Name], typeClassMembers :: [(Name, ReferentialType)]}
    deriving (Show, Eq, Ord)

data Instance = Instance {instanceType :: ReferentialType, instanceClassName :: Name, instanceMembers :: [(Name, Value)]}
    deriving (Show, Eq, Ord)

data Definition = Definition {definitionName :: Name, definitionType :: ReferentialType, definitionValue :: Value}
    deriving (Show, Eq, Ord)

data TypeAlias = TypeAlias {aliasName :: Name, aliasArgumentCount :: Int, aliasType :: Maybe ReferentialType}
    deriving (Show, Eq, Ord)

data Name = Name {nameOffset :: ParsingOffset, nameString :: String}
    deriving (Ord)

instance IsString Name where
    fromString = Name 0

data Value
    = ProductLiteral ParsingOffset Value Value
    | SumLiteral ParsingOffset Bool Value
    | BoolLiteral ParsingOffset Bool
    | IntLiteral ParsingOffset Int
    | FloatLiteral ParsingOffset Double
    | CharLiteral ParsingOffset Char
    | EmptyTupleLiteral ParsingOffset
    | DefinedValue Name
    | DefinedValueFromInstance Name (Either Int ReferentialType)
    | Undefined ParsingOffset
    | UnaryArrowOperator UnaryArrowOperator ParsingOffset (Maybe ReferentialType) Value
    | BinaryArrowOperator BinaryArrowOperator ParsingOffset (Maybe ReferentialType) Value Value
    -- | Arr ParsingOffset Value
    -- | ArrowComposition ParsingOffset Value Value
    -- | ArrowConstant ParsingOffset Value
    -- | ArrowFirst ParsingOffset Value
    -- | ArrowSecond ParsingOffset Value
    -- | TripleAsterisks ParsingOffset Value Value
    -- | TripleAnd ParsingOffset Value Value
    -- | ArrowRight ParsingOffset Value
    -- | ArrowLeft ParsingOffset Value
    -- | TriplePlus ParsingOffset Value Value
    -- | TripleBar ParsingOffset Value Value
    deriving (Eq, Ord, Show)

data UnaryArrowOperator = Arr | ArrowConstant | ArrowFirst | ArrowSecond | ArrowRight | ArrowLeft
    deriving (Eq, Ord)

data BinaryArrowOperator = ArrowComposition | TripleAsterisks | TripleAnd | TriplePlus | TripleBar
    deriving (Eq, Ord)

data TypeWithValue
    = TypeWithProductLiteral Type TypeWithValue TypeWithValue
    | TypeWithSumLiteral Type Bool TypeWithValue
    | TypeWithBoolLiteral Type Bool
    | TypeWithIntLiteral Type Int
    | TypeWithFloatLiteral Type Double
    | TypeWithCharLiteral Type Char
    | TypeWithEmptyTupleLiteral Type
    | TypeWithDefinedValue Type Name
    | TypeWithDefinedValueFromInstance Type Name Int
    | TypeWithUndefined Type
    | TypeWithUnaryArrowOperator UnaryArrowOperator Type TypeWithValue
    | TypeWithBinaryArrowOperator BinaryArrowOperator Type TypeWithValue TypeWithValue

    -- | TypeWithArr Type TypeWithValue
    -- | TypeWithArrowComposition Type TypeWithValue TypeWithValue
    -- | TypeWithArrowConstant Type TypeWithValue
    -- | TypeWithArrowFirst Type TypeWithValue
    -- | TypeWithArrowSecond Type TypeWithValue
    -- | TypeWithTripleAsterisks Type TypeWithValue TypeWithValue
    -- | TypeWithTripleAnd Type TypeWithValue TypeWithValue
    -- | TypeWithArrowRight Type TypeWithValue
    -- | TypeWithArrowLeft Type TypeWithValue
    -- | TypeWithTriplePlus Type TypeWithValue TypeWithValue
    -- | TypeWithTripleBar Type TypeWithValue TypeWithValue
    deriving (Eq, Ord, Show)

data ReferentialType = ReferentialType {mainType :: Type, otherTypes :: [Type]}
    deriving (Ord, Show)

resolveReference :: ReferentialType -> Int -> ReferentialType
resolveReference (ReferentialType _ references) index = ReferentialType (references !! index) references

makeTypeReferential :: Type -> ReferentialType
makeTypeReferential t = ReferentialType t []

type ParsingOffset = Int

data Type
    = Product Type Type
    | Sum Type Type
    | ForAllInstances Int [Name]
    | AnyType Int [Name]
    | AliasReference Name [Type]
    | AliasExtention Int [Type]
    | TypeReference Int
    | ThisClass
    --    | Bool
    --    | Float
    --    | Int
    --    | Char
    --    | EmptyTuple
    deriving (Eq, Ord, Show)

instance Eq Name where
    (Name _ x) == (Name _ y) = x == y

instance Show Name where
    show = show . nameString

instance Show UnaryArrowOperator where
    show Arr = "arr"
    show ArrowConstant = "const"
    show ArrowFirst = "first"
    show ArrowSecond = "second"
    show ArrowRight = "right"
    show ArrowLeft = "left"

instance Show BinaryArrowOperator where
    show ArrowComposition = ">>>"
    show TripleAsterisks = "***" 
    show TripleAnd = "&&&"
    show TriplePlus = "+++"
    show TripleBar = "|||"

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
                (AliasReference nameX argumentsX) -> eq (AliasReference nameX (argumentsX ++ arguments)) y
                _ -> return False
        eq x (AliasExtention index arguments) = do
            y <- gets $ (!! index) . fst
            case y of
                (AliasReference nameY argumentsY) -> eq x (AliasReference nameY (argumentsY ++ arguments))
                _ -> return False
        eq (Product x1 x2) (Product y1 y2) = do
            eq1 <- eq x1 y1
            eq2 <- eq x2 y2
            return $ eq1 && eq2
        eq (Sum x1 x2) (Sum y1 y2) = do
            eq1 <- eq x1 y1
            eq2 <- eq x2 y2
            return $ eq1 && eq2
        eq (ForAllInstances _ unsortedClassesX) (ForAllInstances _ unsortedClassesY) = do
            let classesX = sort unsortedClassesX
            let classesY = sort unsortedClassesY
            return $ classesX == classesY
        eq (AnyType _ unsortedClassesX) (AnyType _ unsortedClassesY) = do
            let classesX = sort unsortedClassesX
            let classesY = sort unsortedClassesY
            return $ classesX == classesY
        eq (AliasReference nameX argumentsX) (AliasReference nameY argumentsY) = do
            if nameX == nameY && length argumentsX == length argumentsY
                then and <$> zipWithM eq argumentsX argumentsY
                else return False
        eq ThisClass ThisClass = return True
        eq x y = return $ x == y
