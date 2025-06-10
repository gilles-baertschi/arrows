{-# LANGUAGE OverloadedStrings #-}

module Parser.Types (typeP, aliasP, classP, typeConstraintsP, typeWithExistingConstraintsP, Constraint (..), typeTailP, toReferentialType, constrainedTypeP) where

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
        (ReferentialType memberMainType memberOtherTypes) <- typeWithExistingConstraintsP constraints
        return (memberName, ReferentialType memberMainType $ ThisClass : tail memberOtherTypes)
    p = do
        _ <- symbol "class"
        maybeConstraints <- try (Just <$> typeConstraintsP) <|> return Nothing
        className <- upperCaseNameP
        localName <- lowerCaseNameP
        unless (maybe True (all ((localName ==) . constrainedLocalName)) maybeConstraints) $ fail ("only " ++ nameString localName ++ " allowed in constraints")
        let parents = constrainedLocalName <$> fromMaybe [] maybeConstraints
        let constraints = mergeConstraints $ Constraint localName [className] : fromMaybe [] maybeConstraints
        _ <- symbol "where"
        return $ L.IndentSome Nothing (return . TypeClass className parents) (member constraints)

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
            then do
                let termianlP = symbol "undefined" $> TypeAlias name argumentCount Nothing
                let normalP = TypeAlias name argumentCount . Just . toReferentialType <$> runStateT (typeTailP True) (map (`Constraint` []) arguments)
                (termianlP <|> normalP) <* newLineP
            -- result <- symbol "undefined" $> TerminalTypeAlias name argumentCount <|> TypeAlias name argumentCount . toReferentialType <$> runStateT (typeTailP True) (map (`Constraint` []) arguments)
            -- _ <- newLineP
            -- return result
            else fail "several definitions for the same type variable"

typeP :: Parser ReferentialType
typeP = typeWithExistingConstraintsP []

typeWithExistingConstraintsP :: [Constraint] -> Parser ReferentialType
typeWithExistingConstraintsP constraints = toReferentialType <$> runStateT (((try (lift typeConstraintsP) >>= put) <|> return ()) >> typeTailP True) constraints

toReferentialType :: (Type, [Constraint]) -> ReferentialType
toReferentialType (finalType, constraints) = ReferentialType finalType $ map (\(Constraint _ classNames) -> ForAllInstances classNames) constraints

typeConstraintsP :: Parser [Constraint]
typeConstraintsP = (((: []) <$> singleConstraintP) <|> (mergeConstraints <$> parens (sepBy singleConstraintP (symbol ",")))) <* symbol "=>"
  where
    singleConstraintP = flip Constraint . (: []) <$> upperCaseNameP <*> lowerCaseNameP

-- let allLocalNames = nub $ map constrainedClassNames bindings
-- let bindingsWithLocalName name = map fst $ filter ((name ==) . snd) bindings
-- return $ map (\name -> Constraint name (bindingsWithLocalName name)) allLocalNames

mergeConstraints :: [Constraint] -> [Constraint]
mergeConstraints constraints = map (\name -> Constraint name (constraintsWithLocalName name)) allLocalNames
  where
    allLocalNames = nub $ map constrainedLocalName constraints
    constraintsWithLocalName name = concatMap constrainedClassNames $ filter ((name ==) . constrainedLocalName) constraints

typeTailP :: Bool -> ParserWithState [Constraint] Type
typeTailP inParens = makeExprParser (typeTermP inParens) (typeOperatorTable inParens)

typeTermP :: Bool -> ParserWithState [Constraint] Type
typeTermP inParens =
    choice
        [ -- lift $ symbol "Bool" $> AliasReference (Name "Bool" []
          -- , lift $ symbol "Char" $> AliasReference "Char" []
          -- , lift $ symbol "Int" $> AliasReference "Int" []
          -- , lift $ symbol "Float" $> AliasReference "Float" []
          -- , lift $ symbol "()" $> AliasReference "()" []
          try $ AliasReference <$> lift upperCaseNameP <*> if inParens then many (try $ typeTailP False) else return []
        , constrainedTypeP inParens
        , parensWithState $ typeTailP True
        ]

constrainedTypeP :: Bool -> ParserWithState [Constraint] Type
constrainedTypeP inParens = try $ do
    name <- lift lowerCaseNameP
    arguments <- if inParens then many $ typeTailP False else return []
    let result index = if not $ null arguments then AliasExtention index arguments else TypeReference index
    constraints <- get
    let maybeIndex = elemIndex name (map constrainedLocalName constraints)
    case maybeIndex of
        Nothing -> do
            modify (++ [Constraint name []])
            let index = length constraints
            return $ result index
        Just index -> return $ result index

typeOperatorTable :: Bool -> [[Operator (ParserWithState [Constraint]) Type]]
typeOperatorTable inParens =
    if inParens
        then
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
        else []
