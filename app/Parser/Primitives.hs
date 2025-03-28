{-# LANGUAGE OverloadedStrings #-}

module Parser.Primitives where

import Control.Monad.Combinators.Expr
import Control.Monad.State
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

type ParserWithState s = StateT s Parser

newLineP :: Parser ()
newLineP = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

spaceP :: Parser ()
spaceP = L.space (void $ some (char ' ' <|> char '\t')) (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceP

symbol :: Text -> Parser Text
symbol = L.symbol spaceP

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parensWithState :: ParserWithState s a -> ParserWithState s a
parensWithState = between (lift $ symbol "(") (lift $ symbol ")")

charLiteralP :: Parser Char
charLiteralP = between (char '\'') (char '\'') L.charLiteral

stringLiteralP :: Parser String
stringLiteralP = char '\"' *> manyTill L.charLiteral (char '\"')

signedIntegerP :: Parser Integer
signedIntegerP = L.signed spaceP $ lexeme L.decimal

signedFloatP :: Parser Double
signedFloatP = L.signed spaceP $ lexeme L.float

nameP :: Parser String
nameP = lexeme $ (:) <$> letterChar <*> many alphaNumChar

lowerCaseNameP :: Parser String
lowerCaseNameP = lexeme $ (:) <$> lowerChar <*> many alphaNumChar

upperCaseNameP :: Parser String
upperCaseNameP = lexeme $ (:) <$> upperChar <*> many alphaNumChar

binary :: Text -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (a -> a) -> Operator Parser a
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

binaryWithState :: Text -> (a -> a -> a) -> Operator (ParserWithState s) a
binaryWithState name f = InfixL (f <$ lift (symbol name))

prefixWithState, postfixWithState :: Text -> (a -> a) -> Operator (ParserWithState s) a
prefixWithState name f = Prefix (f <$ lift (symbol name))
postfixWithState name f = Postfix (f <$ lift (symbol name))
