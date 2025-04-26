module Parser.Program where

import Ast
import Checker.Names
import Checker.Types
import Control.Monad.State
import Data.Text (pack)
import Parser.Primitives
import Parser.Types
import Parser.Values
import Text.Megaparsec

checkAll :: ParserWithState Program ()
checkAll = checkNameSafety >> checkTypeSafety

programP :: Parser Program
programP = execStateT (many (choice [writeClassP, writeInstanceP, writeAliasP, writeDefinitionP]) >> checkAll) (Program [] [] [] []) <* eof

writeAliasP :: ParserWithState Program ()
writeAliasP = do
    newAlias <- lift aliasP
    _ <- lift newLineP
    existingAliases <- gets aliases
    modify $ \program -> program{aliases = existingAliases ++ [newAlias]}

writeInstanceP :: ParserWithState Program ()
writeInstanceP = do
    newInstance <- lift instanceP
    _ <- lift newLineP
    existingInstances <- gets instances
    modify $ \program -> program{instances = existingInstances ++ [newInstance]}

writeClassP :: ParserWithState Program ()
writeClassP = do
    newClass <- lift classP
    _ <- lift newLineP
    existingClasses <- gets typeClasses
    modify $ \program -> program{typeClasses = existingClasses ++ [newClass]}

writeDefinitionP :: ParserWithState Program ()
writeDefinitionP = do
    name <- lift lowerCaseNameP
    _ <- lift $ symbol "::"
    referentialType <- lift typeP
    _ <- lift newLineP
    _ <- lift $ symbol $ pack name
    _ <- lift $ symbol "="
    value <- lift valueP
    _ <- lift newLineP
    let newDefinition = Definition{definitionValue = value, definitionType = referentialType, definitionName = name}
    existingDefinitions <- gets definitions
    modify $ \program -> program{definitions = existingDefinitions ++ [newDefinition]}
