module Checker.Names where

import Ast
import Control.Monad
import Control.Monad.State
import Data.Functor
import Helpers
import Parser.Primitives
import Text.Megaparsec

checkNameSafety :: ParserWithState Program ()
checkNameSafety = do
    (Program _ _ programDefinitions programAliases) <- get
    mapM_ (checkTypeNameSafety . definitionType) programDefinitions
    mapM_ (checkTypeNameSafety . aliasType) programAliases
    mapM_ (checkValueNameSafety . definitionValue) programDefinitions

checkTypeNameSafety :: ReferentialType -> ParserWithState Program ()
checkTypeNameSafety referentialType@(ReferentialType t references) = check t >> mapM check references $> ()
  where
    check :: Type -> ParserWithState Program ()
    check (Product x y) = check x >> check y
    check (Sum x y) = check x >> check y
    check (AliasReference offset name arguments) = do
        possibleNames <- gets (map aliasName . aliases)
        unless (name `elem` possibleNames) $ lift (setOffset offset) >> fail ("no alias named " ++ name)
        requiredArgumentCount <- aliasArgumentCount <$> getAlias name
        unless (requiredArgumentCount == length arguments) $ lift (setOffset offset) >> fail ("expected " ++ show requiredArgumentCount ++ " arguments but recieved " ++ show (length name))
        mapM_ check arguments
    check (TypeReference i) = do
        check $ mainType $ resolveReference referentialType i
    check (ForAllInstances classes) = do
        possibleNames <- gets (map typeClassName . typeClasses)
        mapM_ (\(offset, name) -> unless (name `elem` possibleNames) $ lift (setOffset offset) >> fail ("no class named " ++ name)) classes
    check _ = return ()

checkValueNameSafety :: Value -> ParserWithState Program ()
checkValueNameSafety = check
  where
    check :: Value -> ParserWithState Program ()
    check (ProductLiteral _ x y) = check x >> check y
    check (SumLiteral _ x y) = check x >> check y
    check (DefinedValue offset name) = do
        possibleDefinitionNames <- gets $ map definitionName . definitions
        possibleInstanceNames <- gets $ map fst . typeClassMembers <=< typeClasses
        unless (name `elem` (possibleDefinitionNames ++ possibleInstanceNames)) $ lift (setOffset offset) >> fail ("no definition for " ++ name)
    check (ArrowComposition _ x y) = check x >> check y
    check (ArrowConstant _ x) = check x
    check (ArrowFirst _ x) = check x
    check (ArrowSecond _ x) = check x
    check (TripleAsterisks _ x y) = check x >> check y
    check (TripleAnd _ x y) = check x >> check y
    check (ArrowRight _ x) = check x
    check (ArrowLeft _ x) = check x
    check (TriplePlus _ x y) = check x >> check y
    check (TripleBar _ x y) = check x >> check y
    check _ = return ()
