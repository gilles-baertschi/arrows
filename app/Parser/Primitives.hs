{-# LANGUAGE OverloadedStrings #-}

module Parser.Primitives where

import Ast
import Control.Monad.State
import Control.Monad
import Data.Char
import Data.Functor
import Data.Text hiding (elem)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

type ParserWithState s = StateT s Parser

type ParserWithDoubleState s t = StateT s (StateT t Parser)

nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented newLineP

newLineP :: Parser ()
newLineP = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

spaceP :: Parser ()
spaceP = L.space (void $ some (char ' ' <|> char '\t')) (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceP

symbol, alphaNumCharSymbol :: Text -> Parser Text
symbol = L.symbol spaceP
alphaNumCharSymbol x = try $ L.symbol (notFollowedBy alphaNumChar *> spaceP) x

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

liftParens :: ParserWithState s a -> ParserWithState s a
liftParens = between (lift $ symbol "(") (lift $ symbol ")")

parensWithState :: ParserWithState s a -> ParserWithState s a
parensWithState = between (lift $ symbol "(") (lift $ symbol ")")

charLiteralP :: Parser Char
charLiteralP = lexeme (between (char '\'') (char '\'') L.charLiteral) <|> symbol "'\\n'" $> '\n'

stringLiteralP :: Parser String
stringLiteralP = char '\"' *> manyTill L.charLiteral (char '\"')

signedIntegerP :: Parser Integer
signedIntegerP = L.signed spaceP $ lexeme L.decimal

signedFloatP :: Parser Double
signedFloatP = L.signed spaceP $ lexeme L.float

-- nameP :: Parser String
-- nameP = lexeme $ (:) <$> letterChar <*> many alphaNumChar

lowerCaseNameP :: Parser Name
lowerCaseNameP = lexeme $ Name <$> getOffset <*> lowerCaseStringP
  where
    lowerCaseStringP = specialStringP <|> (((:) <$> lowerChar <*> many (satisfy (\x -> isAlphaNum x || x == '_'))) >>= notKeyword)

upperCaseNameP :: Parser Name
upperCaseNameP = lexeme $ Name <$> getOffset <*> upperCaseStringP
  where
    upperCaseStringP = specialStringP <|> (((:) <$> upperChar <*> many (satisfy (\x -> isAlphaNum x || x == '_'))) >>= notKeyword)

notKeyword :: String -> Parser String
notKeyword name = do
    when (name `elem` keywords) $ fail "not expecting keyword"
    return name

specialStringP :: Parser String
specialStringP = (symbol "()" $> "()") <|> parens (some specialCharP) 
  where
    specialCharP = satisfy (\c -> (isPunctuation c || isSymbol c) && c /= '(' && c /= ')')

-- binaryL, binaryR, binaryN :: Text -> Parser (a -> a -> a) -> Operator Parser a
-- binaryL name f = InfixL (f <* symbol name)
-- binaryR name f = InfixR (f <* symbol name)
-- binaryN name f = InfixN (f <* symbol name)
--
-- prefix, postfix :: Text -> Parser (a -> a) -> Operator Parser a
-- prefix name f = Prefix (f <* symbol name)
-- postfix name f = Postfix (f <* symbol name)
--
-- binaryWithState :: Text -> (a -> a -> a) -> Operator (ParserWithState s) a
-- binaryWithState name f = InfixL (f <$ lift (symbol name))
--
-- prefixWithState, postfixWithState :: Text -> (a -> a) -> Operator (ParserWithState s) a
-- prefixWithState name f = Prefix (f <$ lift (symbol name))
-- postfixWithState name f = Postfix (f <$ lift (symbol name))

keywords :: [String]
keywords = ["where", "instance", "class", "undefined", "->", ",", "|"] -- ["where", "instance", "class", "arr", "first", "second", "left", "right", "const", "***", "&&&", "+++", "|||", ",", "|"]
