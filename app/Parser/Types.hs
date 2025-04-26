{-# LANGUAGE OverloadedStrings #-}

module Parser.Types (typeP, aliasP, classP, typeConstraintsP, typeWithExistingConstraintsP, Constraint (..), typeTailP, toReferentialType) where

import Ast
import Control.Monad.Combinators.Expr
import Control.Monad.State
import Data.Functor
import Data.List
import Data.List.Unique
import Data.Maybe
import Parser.Primitives
import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.Char.Lexer as L

data Constraint = Constraint {constrainedLocalName :: String, constrainedClassNames :: [(ParsingOffset, String)]}

-- pItemList :: Parser (String, [String])
-- pItemList = L.nonIndented newLineP (L.indentBlock newLineP p)
-- where
-- p = do
-- header <- pItem
-- return (L.IndentMany Nothing (return . (header,)) pItem)
--
classP :: Parser TypeClass
classP = L.nonIndented newLineP (L.indentBlock newLineP p)
  where
    member constraints = do
        -- _ <- newLineP
        memberName <- lowerCaseNameP
        _ <- symbol "::"
        memberType <- typeWithExistingConstraintsP constraints
        return (memberName, memberType)
    p = do
        _ <- symbol "class"
        maybeConstraints <- try (Just <$> typeConstraintsP) <|> return Nothing
        className <- upperCaseNameP
        -- (localName, partialArguments) <- ((,) <$> lowerCaseNameP <*> return []) <|> parens ((,) <$> lowerCaseNameP <*> many lowerCaseNameP)
        localName <- lowerCaseNameP
        unless (maybe True (all ((localName ==) . constrainedLocalName)) maybeConstraints) $ fail ("only " ++ localName ++ " allowed in constraints")
        let constraints = fromMaybe [Constraint localName []] maybeConstraints
        _ <- symbol "where"
        -- _ <- newLineP
        -- members <- some $ member constraints
        return $ L.IndentSome Nothing (return . TypeClass className) (member constraints)

aliasP :: Parser TypeAlias
aliasP = do
    _ <- symbol "type"
    name <- upperCaseNameP
    arguments <- many lowerCaseNameP
    _ <- symbol "="
    let argumentCount = length arguments
    if allUnique arguments
        then TypeAlias name argumentCount . toReferentialType <$> runStateT typeTailP (map (`Constraint` []) arguments)
        else fail "several definitions for the same type variable"

typeP :: Parser ReferentialType
typeP = typeWithExistingConstraintsP []

typeWithExistingConstraintsP :: [Constraint] -> Parser ReferentialType
typeWithExistingConstraintsP constraints = toReferentialType <$> runStateT (((try (lift typeConstraintsP) >>= put) <|> return ()) >> typeTailP) constraints

toReferentialType :: (Type, [Constraint]) -> ReferentialType
toReferentialType (finalType, constraints) = ReferentialType finalType $ map (\(Constraint _ classNames) -> ForAllInstances classNames) constraints

typeConstraintsP :: Parser [Constraint]
typeConstraintsP = do
    bindings <- parens (sepBy ((,,) <$> getOffset <*> upperCaseNameP <*> lowerCaseNameP) (symbol ",")) <* symbol "=>"
    let allLocalNames = nub $ map (\(_, _, localName) -> localName) bindings
    let bindingsWithLocalName name = map (\(offset, className, _) -> (offset, className)) $ filter (\(_, _, localName) -> name == localName) bindings
    return $ map (\name -> Constraint name (bindingsWithLocalName name)) allLocalNames

-- return (map ((`InstanceOf` []) . map (\(offset, className, _) -> (offset, className)) . constraintsWithLocalName) classNames, classNames)

typeTailP :: ParserWithState [Constraint] Type
typeTailP = makeExprParser typeTermP typeOperatorTable

typeTermP :: ParserWithState [Constraint] Type
typeTermP =
    choice
        [ lift $ symbol "Bool" $> Bool
        , lift $ symbol "Char" $> Char
        , lift $ symbol "Int" $> Int
        , lift $ symbol "Float" $> Float
        , lift $ symbol "()" $> EmptyTuple
        , AliasReference <$> getOffset <*> lift upperCaseNameP <*> many typeTailP
        , constrainedTypeP
        , parensWithState typeTailP
        ]

constrainedTypeP :: ParserWithState [Constraint] Type
constrainedTypeP = try $ do
    name <- lift lowerCaseNameP
    arguments <- many typeTailP
    constraints <- get
    let maybeIndex = elemIndex name (map constrainedLocalName constraints)
    case maybeIndex of
        Nothing -> do
            put $ constraints ++ [Constraint name []] -- ( ++ [InstanceOf [] []], translations ++ [name])
            return $ AliasExtention (length constraints) arguments
        Just index -> return $ AliasExtention index arguments

typeOperatorTable :: [[Operator (ParserWithState [Constraint]) Type]]
typeOperatorTable =
    [
        [ InfixL
            ( do
                offset <- getOffset
                _ <- lift (symbol "->")
                return (\x y -> AliasReference offset "Id" [x, y])
            )
        ]
    , [binaryWithState "," Product]
    , [binaryWithState "|" Sum]
    ]
