{-# LANGUAGE OverloadedStrings #-}

module Parser.Types where

import Ast
import Control.Monad.Combinators.Expr
import Control.Monad.State
import Data.Functor
import Data.List
import Parser.Primitives
import Text.Megaparsec hiding (State)

-- classP :: Parser TypeClass
-- classP = do
--   _ <- symbol "class"
-- (references, translations) <- execStateT (try typeConstraintP <|> return ()) ([], [])
--  name <- upperCaseNameP
--  arguments <- map (fromMaybe (InstanceOf []) . fmap (references !!) . flip elemIndex translations) <$> many lowerCaseNamePi
--  return $ TypeClass{className = name, classMembers = arguments}

aliasP :: Parser TypeAlias
aliasP = do
    name <- upperCaseNameP
    constraints <- many lowerCaseNameP
    _ <- symbol "="
    let constraintCount = length constraints
    if nub constraints == constraints
        then TypeAlias name constraintCount <$> runTypeParserWithState typeTailP (replicate constraintCount $ InstanceOf [], constraints)
        else fail "several definitions for same variable"

typeP :: Parser ReferentialType
typeP = runTypeParserWithState ((try typeConstraintP <|> return ()) >> typeTailP) ([], [])

runTypeParserWithState :: ParserWithState ReferencesWithTranslations Type -> ReferencesWithTranslations -> Parser ReferentialType
runTypeParserWithState p s = (\(finalType, (references, _)) -> ReferentialType finalType references) <$> runStateT p s

type ReferencesWithTranslations = ([Type], [String])

typeConstraintP :: ParserWithState ReferencesWithTranslations ()
typeConstraintP = do
    constraints <- lift $ parens (sepBy ((,) <$> upperCaseNameP <*> lowerCaseNameP) (symbol ",")) <* symbol "=>"
    let translation = nub $ map snd constraints
    let constraintsWithName name = filter (\(_, name') -> name == name') constraints
    put (map (InstanceOf . map fst . constraintsWithName) translation, translation)

typeTailP :: ParserWithState ReferencesWithTranslations Type
typeTailP = makeExprParser typeTermP typeOperatorTable

typeTermP :: ParserWithState ReferencesWithTranslations Type
typeTermP =
    choice
        [ lift $ symbol "Bool" $> Bool
        , lift $ symbol "Char" $> Char
        , lift $ symbol "Int" $> Int
        , lift $ symbol "Float" $> Float
        , lift $ symbol "()" $> EmptyTuple
        , AliasReference <$> lift upperCaseNameP <*> many typeTailP
        , constrainedTypeP
        , parensWithState typeTailP
        ]

constrainedTypeP :: ParserWithState ReferencesWithTranslations Type
constrainedTypeP = try $ do
    (references, translations) <- get
    name <- lift lowerCaseNameP
    let maybeIndex = elemIndex name translations
    case maybeIndex of
        Nothing -> do
            put (references ++ [InstanceOf []], translations ++ [name])
            return $ TypeReference $ length references
        Just index -> return $ TypeReference index

typeOperatorTable :: [[Operator (ParserWithState ReferencesWithTranslations) Type]]
typeOperatorTable =
    [ [binaryWithState "->" (\x y -> AliasReference "Id" [x, y])]
    , [binaryWithState "," Product]
    , [binaryWithState "|" Sum]
    ]
