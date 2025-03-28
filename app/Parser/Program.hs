module Parser.Program where

import Ast
import Control.Monad.State
import Data.Text (pack)
import Parser.Primitives
import Parser.Types
import Parser.Values (valueP)
import Text.Megaparsec

programP :: Parser Program
programP = execStateT (many (choice [writeAliasP, writeDefinitionP])) (Program [] [] [] []) <* eof

writeAliasP :: ParserWithState Program ()
writeAliasP = do
    _ <- lift $ symbol "type"
    newAlias <- lift aliasP
    _ <- lift newLineP
    existingAliases <- gets aliases
    modify $ \program -> program{aliases = existingAliases ++ [newAlias]}

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
