module Checker.Names where

import Ast
import Control.Monad
import Control.Monad.State
import Data.Foldable (find)
import Data.Functor
import Data.List.Unique
import Data.Maybe
import Helpers
import Parser.Primitives
import Text.Megaparsec

data Names = Names {classNames :: [Name], aliasNames :: [Name], valueNames :: [Name]}

checkNameSafety :: ParserWithState Program ()
checkNameSafety = void $ runStateT f (Names [] [] [])
  where
    f = do
        (Program programTypeClasses programInstances programDefinitions programAliases) <- lift get
        _ <- putPossibleNames
        mapM_ assertInstanceNameSafety programInstances
        mapM_ (assertTypeNameSafety True . definitionType) programDefinitions
        mapM_ (assertTypeNameSafety True) $ mapMaybe aliasType programAliases
        mapM_ (assertTypeNameSafety False . instanceType) programInstances
        mapM_ (assertTypeNameSafety True . snd) $ concatMap typeClassMembers programTypeClasses
        mapM_ (assertValueNameSafety . definitionValue) programDefinitions
        mapM_ (assertValueNameSafety . snd) $ concatMap instanceMembers programInstances
        unless (isJust $ find ((== "main") . nameString . definitionName) programDefinitions) $ fail "no definition for main"

putPossibleNames :: ParserWithDoubleState Names Program ()
putPossibleNames = putPossibleClassNames >> putPossibleAliasNames >> putPossibleValueNames

putPossibleClassNames :: ParserWithDoubleState Names Program ()
putPossibleClassNames = do
    allClassNames <- lift $ gets $ map typeClassName . typeClasses
    mapM_ f allClassNames
  where
    f name = do
        assertNewName name
        existingClassNames <- gets classNames
        modify (\names -> names{classNames = name : existingClassNames})

putPossibleAliasNames :: ParserWithDoubleState Names Program ()
putPossibleAliasNames = do
    allAliasNames <- lift $ gets $ map aliasName . aliases
    mapM_ f allAliasNames
  where
    f name = do
        assertNewName name
        existingAliasNames <- gets aliasNames
        modify (\names -> names{aliasNames = name : existingAliasNames})

putPossibleValueNames :: ParserWithDoubleState Names Program ()
putPossibleValueNames = do
    allClassMemberNames <- lift $ gets $ map fst . typeClassMembers <=< typeClasses
    allDefinitionNames <- lift $ gets $ map definitionName . definitions
    mapM_ f (allClassMemberNames ++ allDefinitionNames)
  where
    f name = do
        assertNewName name
        existingValueNames <- gets valueNames
        modify (\names -> names{valueNames = name : existingValueNames})

assertClassNameExist :: Name -> ParserWithDoubleState Names Program ()
assertClassNameExist name = do
    exists <- gets $ (name `elem`) . classNames
    unless exists $ do
        setOffset $ nameOffset name
        fail $ "no class named " ++ nameString name

assertAliasNameExist :: Name -> ParserWithDoubleState Names Program ()
assertAliasNameExist name = do
    exists <- gets $ (name `elem`) . aliasNames
    unless exists $ do
        setOffset $ nameOffset name
        fail $ "no alias named " ++ nameString name

assertValueNameExist :: Name -> ParserWithDoubleState Names Program ()
assertValueNameExist name = do
    exists <- gets $ (name `elem`) . valueNames
    unless exists $ do
        setOffset $ nameOffset name
        fail $ "no definition for " ++ nameString name

doesNameExist :: Name -> ParserWithDoubleState Names Program Bool
doesNameExist name = (choice [assertClassNameExist name, assertAliasNameExist name, assertValueNameExist name] $> True) <|> return False

assertNewName :: Name -> ParserWithDoubleState Names Program ()
assertNewName name = do
    exists <- doesNameExist name
    when exists $ do
        setOffset $ nameOffset name
        fail $ "multiple definitions for " ++ nameString name

assertTypeNameSafety :: Bool -> ReferentialType -> ParserWithDoubleState Names Program ()
assertTypeNameSafety strictArgumentCounts referentialType@(ReferentialType t references) = do
    check t
    mapM_
        ( \reference -> case reference of
            (AliasReference _ _) -> return ()
            _ -> check reference
        )
        references
  where
    check :: Type -> ParserWithDoubleState Names Program ()
    check (Product x y) = check x >> check y
    check (Sum x y) = check x >> check y
    check (AliasExtention index extendedArguments) = case references !! index of
        (AliasReference name@(Name offset _) coreArguments) -> do
            assertAliasNameExist name
            let arguments = coreArguments ++ extendedArguments
            requiredArgumentCount <- aliasArgumentCount <$> lift (getAlias name)
            unless (requiredArgumentCount == length arguments || not strictArgumentCounts) $ do
                lift $ setOffset offset
                fail $ "expected " ++ show (requiredArgumentCount - length coreArguments) ++ " arguments but recieved " ++ show (length extendedArguments) ++ show arguments
            mapM_ check arguments
        (ForAllInstances _ []) -> return ()
        (ForAllInstances _ [_]) -> return ()
        ThisClass -> return ()
        _ -> undefined
    check (AliasReference name@(Name offset _) arguments) = do
        assertAliasNameExist name
        requiredArgumentCount <- aliasArgumentCount <$> lift (getAlias name)
        unless (requiredArgumentCount == length arguments || not strictArgumentCounts) $ do
            setOffset offset
            fail $ "expected " ++ show requiredArgumentCount ++ " arguments but recieved " ++ show (length arguments)
        mapM_ check arguments
    check (TypeReference index) = do
        check $ mainType $ resolveReference referentialType index
    check (ForAllInstances _ classes) = do
        mapM_ assertClassNameExist classes
    check _ = return ()

assertValueNameSafety :: Value -> ParserWithDoubleState Names Program ()
assertValueNameSafety = check
  where
    check :: Value -> ParserWithDoubleState Names Program ()
    check (ProductLiteral _ x y) = check x >> check y
    check (SumLiteral _ _ x) = check x
    check (DefinedValue name) = assertValueNameExist name
    check (DefinedValueFromInstance name (Right instancedType)) = assertValueNameExist name >> assertTypeNameSafety False instancedType
    check (BinaryArrowOperator _ _ _ x y) = check x >> check y
    check (UnaryArrowOperator _ _ _ x) = check x
    check _ = return ()

assertInstanceNameSafety :: Instance -> ParserWithDoubleState Names Program ()
assertInstanceNameSafety (Instance _ className members) = do
    assertClassNameExist className
    let memberNames = map fst members
    requiredMemberNames <- lift $ gets $ map fst . typeClassMembers . head . filter ((className ==) . typeClassName) . typeClasses
    unless (allUnique memberNames) $ do
        setOffset $ nameOffset className
        fail "multiple definitions for same member of instance"
    mapM_
        ( \name -> unless (name `elem` memberNames) $ do
            setOffset $ nameOffset className
            fail $ nameString name ++ " was not defined in an instance of " ++ nameString className
        )
        requiredMemberNames
    mapM_
        ( \name -> unless (name `elem` requiredMemberNames) $ do
            setOffset $ nameOffset name
            fail $ nameString name ++ " is not part of the class " ++ nameString className
        )
        memberNames
