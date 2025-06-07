module Parser.Program where

import Ast
import Checker.Names
import Checker.Types
import Control.Monad.State
import Parser.Primitives
import Parser.Types
import Parser.Values
import Text.Megaparsec
import Translator

checkAll :: ParserWithState Program ()
checkAll = checkNameSafety >> checkTypeSafety

onlyProgramP :: Parser Program
onlyProgramP = execStateT p (Program [] [] [] []) <* eof
  where
    p = do
        _ <- lift newLineP
        _ <- many $ choice [writeClassP, writeInstanceP, writeAliasP, writeDefinitionP]
        checkAll

programP :: Parser String
programP = evalStateT p (Program [] [] [] []) <* eof
  where
    p = do
        _ <- lift newLineP
        _ <- many $ choice [writeClassP, writeInstanceP, writeAliasP, writeDefinitionP]
        checkAll
        translate

writeAliasP :: ParserWithState Program ()
writeAliasP = do
    newAlias <- lift aliasP
    existingAliases <- gets aliases
    modify $ \program -> program{aliases = existingAliases ++ [newAlias]}

writeInstanceP :: ParserWithState Program ()
writeInstanceP = do
    newInstance <- lift instanceP
    existingInstances <- gets instances
    modify $ \program -> program{instances = existingInstances ++ [newInstance]}

writeClassP :: ParserWithState Program ()
writeClassP = do
    newClass <- lift classP
    existingClasses <- gets typeClasses
    modify $ \program -> program{typeClasses = existingClasses ++ [newClass]}

writeDefinitionP :: ParserWithState Program ()
writeDefinitionP = do
    newDefinition <- lift definitionP
    existingDefinitions <- gets definitions
    modify $ \program -> program{definitions = existingDefinitions ++ [newDefinition]}
