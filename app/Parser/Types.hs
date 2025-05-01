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

data Constraint = Constraint {constrainedLocalName :: Name, constrainedClassNames :: [Name]}

classP :: Parser TypeClass
classP = nonIndented (L.indentBlock newLineP p)
  where
    member constraints = do
        memberName <- lowerCaseNameP
        _ <- symbol "::"
        memberType <- typeWithExistingConstraintsP constraints
        return (memberName, memberType)
    p = do
        _ <- symbol "class"
        maybeConstraints <- try (Just <$> typeConstraintsP) <|> return Nothing
        className <- upperCaseNameP
        localName <- lowerCaseNameP
        unless (maybe True (all ((localName ==) . constrainedLocalName)) maybeConstraints) $ fail ("only " ++ nameString localName ++ " allowed in constraints")
        let constraints = mergeConstraints $ Constraint localName [className] : fromMaybe [] maybeConstraints
        _ <- symbol "where"
        return $ L.IndentSome Nothing (return . TypeClass className) (member constraints)

aliasP :: Parser TypeAlias
aliasP = nonIndented p
  where
    p = do
        _ <- symbol "type"
        name <- upperCaseNameP
        arguments <- many lowerCaseNameP
        _ <- symbol "="
        let argumentCount = length arguments
        if allUnique arguments
            then TypeAlias name argumentCount . toReferentialType <$> runStateT typeTailP (map (`Constraint` []) arguments) <* newLineP
            else fail "several definitions for the same type variable"

typeP :: Parser ReferentialType
typeP = typeWithExistingConstraintsP []

typeWithExistingConstraintsP :: [Constraint] -> Parser ReferentialType
typeWithExistingConstraintsP constraints = toReferentialType <$> runStateT (((try (lift typeConstraintsP) >>= put) <|> return ()) >> typeTailP) constraints

toReferentialType :: (Type, [Constraint]) -> ReferentialType
toReferentialType (finalType, constraints) = ReferentialType finalType $ map (\(Constraint _ classNames) -> ForAllInstances classNames) constraints

typeConstraintsP :: Parser [Constraint]
typeConstraintsP = mergeConstraints <$> parens (sepBy (flip Constraint . (: []) <$> upperCaseNameP <*> lowerCaseNameP) (symbol ",")) <* symbol "=>"

-- let allLocalNames = nub $ map constrainedClassNames bindings
-- let bindingsWithLocalName name = map fst $ filter ((name ==) . snd) bindings
-- return $ map (\name -> Constraint name (bindingsWithLocalName name)) allLocalNames

mergeConstraints :: [Constraint] -> [Constraint]
mergeConstraints constraints = map (\name -> Constraint name (constraintsWithLocalName name)) allLocalNames
  where
    allLocalNames = nub $ map constrainedLocalName constraints
    constraintsWithLocalName name = concatMap constrainedClassNames $ filter ((name ==) . constrainedLocalName) constraints

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
        , AliasReference <$> lift upperCaseNameP <*> many typeTailP
        , constrainedTypeP
        , parensWithState typeTailP
        ]

constrainedTypeP :: ParserWithState [Constraint] Type
constrainedTypeP = try $ do
    name <- lift lowerCaseNameP
    arguments <- many typeTailP
    let result index = if not $ null arguments then AliasExtention index arguments else TypeReference index
    constraints <- get
    let maybeIndex = elemIndex name (map constrainedLocalName constraints)
    case maybeIndex of
        Nothing -> do
            modify (++ [Constraint name []])
            let index = length constraints
            return $ result index
        Just index -> return $ result index

typeOperatorTable :: [[Operator (ParserWithState [Constraint]) Type]]
typeOperatorTable =
    [
        [ InfixL
            ( do
                offset <- getOffset
                _ <- lift (symbol "->")
                return (\x y -> AliasReference (Name offset "Id") [x, y])
            )
        ]
    , [binaryWithState "," Product]
    , [binaryWithState "|" Sum]
    ]
