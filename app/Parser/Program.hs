module Parser.Program where

import Ast
import Control.Monad.State
import Parser.Primitives
import Parser.Types
import Parser.Values
import Text.Megaparsec

programP :: Parser Program
programP = execStateT p (Program [] [] [] [])
  where
    p = do
      _ <- lift newLineP
      _ <- many $ choice [writeClassP, writeInstanceP, writeAliasP, writeDefinitionP]
      eof

writeAliasP :: ParserWithState Program ()
writeAliasP = do
  newAlias <- lift aliasP
  existingAliases <- gets aliases
  modify $ \program -> program {aliases = existingAliases ++ [newAlias]}

writeInstanceP :: ParserWithState Program ()
writeInstanceP = do
  newInstance <- lift instanceP
  existingInstances <- gets instances
  modify $ \program -> program {instances = existingInstances ++ [newInstance]}

writeClassP :: ParserWithState Program ()
writeClassP = do
  newClass <- lift classP
  existingClasses <- gets typeClasses
  modify $ \program -> program {typeClasses = existingClasses ++ [newClass]}

writeDefinitionP :: ParserWithState Program ()
writeDefinitionP = do
  newDefinition <- lift definitionP
  existingDefinitions <- gets definitions
  modify $ \program -> program {definitions = existingDefinitions ++ [newDefinition]}
