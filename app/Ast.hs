module Ast (Program (..), TypeClass (..), Instance (..), Definition (..), TypeAlias (..), Value (..), Type (..), ReferentialType (..), resolveReference, makeTypeReferential) where

data Program = Program {typeClasses :: [TypeClass], instances :: [Instance], definitions :: [Definition], aliases :: [TypeAlias]}
    deriving (Read, Eq, Ord, Show)

data TypeClass = TypeClass {className :: String, classMembers :: [(String, Type)]}
    deriving (Read, Eq, Ord, Show)

data Instance = Instance {instanceType :: Type, classNameOf :: String, instanceMembers :: [(String, Value)]}
    deriving (Read, Eq, Ord, Show)

data Definition = Definition {definitionName :: String, definitionType :: ReferentialType, definitionValue :: Value}
    deriving (Read, Eq, Ord, Show)

data TypeAlias = TypeAlias String Int ReferentialType
    deriving (Read, Eq, Ord, Show)

data Value
    = ProductLiteral Value Value
    | SumLiteral Value Value
    | BoolLiteral Bool
    | FloatLiteral Double
    | IntLiteral Int
    | CharLiteral Char
    | EmptyTupleLiteral
    | DefinedValue String
    | AliasInstance String [Value]
    | CompilerDefined
    deriving (Read, Eq, Ord, Show)

data ReferentialType = ReferentialType Type [Type]
    deriving (Read, Eq, Ord, Show)

resolveReference :: ReferentialType -> Int -> ReferentialType
resolveReference (ReferentialType _ references) index = ReferentialType (references !! index) references

makeTypeReferential :: Type -> ReferentialType
makeTypeReferential t = ReferentialType t []

merge :: ReferentialType -> ReferentialType -> Int -> ReferentialType
merge (ReferentialType parent references) child index = ReferentialType parent (references ++ childReferences)
  where
    (ReferentialType innerChild childReferences) = increaseReferences child $ length references
    references' = references !! index

typeGreaterThan :: ReferentialType -> ReferentialType -> Maybe String
typeGreaterThan (ReferentialType greater referencesGreater) (ReferentialType smaller referencesSmaller) = undefined

increaseReferences :: ReferentialType -> Int -> ReferentialType
increaseReferences (ReferentialType t references) index = ReferentialType (increase t) (map increase references)
  where
    increase (Product x y) = Product (increase x) (increase y)
    increase (Sum x y) = Sum (increase x) (increase y)
    increase (AliasReference name types) = AliasReference name $ map increase types
    increase (TypeReference i) = TypeReference $ i + index
    increase x = x

data Type
    = Product Type Type
    | Sum Type Type
    | Bool
    | Float
    | Int
    | Char
    | EmptyTuple
    | InstanceOf [String]
    | AliasReference String [Type]
    | TypeReference Int
    deriving (Read, Eq, Ord, Show)
