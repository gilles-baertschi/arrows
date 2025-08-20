{-# LANGUAGE TupleSections #-}

module Parser.Values (valueP, instanceP, definitionP) where

import Ast
import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Functor
import Data.List
import Data.Text (pack)
import Parser.Primitives
import Parser.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char
import qualified Text.Megaparsec.Char.Lexer as L

definitionP :: Parser Definition
definitionP = nonIndented p
  where
    p = do
        name <- lowerCaseNameP
        _ <- symbol "::"
        referentialType <- typeP
        _ <- newLineP
        _ <- symbol $ pack $ if isAlpha $ head (nameString name) then nameString name else "(" ++ nameString name ++ ")"
        _ <- symbol "="
        value <- valueP True
        _ <- newLineP
        return $ Definition name referentialType value

instanceP :: Parser Instance
instanceP = nonIndented (L.indentBlock newLineP p)
  where
    member = do
        memberName <- lowerCaseNameP
        _ <- symbol "="
        memberValue <- valueP True
        return (memberName, memberValue)
    p = do
        _ <- symbol "instance"
        constraints <- try typeConstraintsP <|> return []
        nameOfClass <- upperCaseNameP
        instancedType <- typeWithExistingConstraintsP constraints
        _ <- "where"
        return $ L.IndentSome Nothing (return . Instance instancedType nameOfClass) member

valueP :: Bool -> Parser Value
valueP inParens = makeExprParser (valueTermP inParens) (valueOperatorTable inParens)

emptyTupleP :: Parser Value
emptyTupleP = EmptyTupleLiteral <$> getOffset <* symbol "()"

boolP :: Parser Value
boolP = BoolLiteral <$> getOffset <*> (symbol "True" $> True <|> symbol "False" $> False)

intP :: Parser Value
intP = do
    offset <- getOffset
    integer <- signedIntegerP
    when (integer < fromIntegral (minBound :: Int) || integer > fromIntegral (maxBound :: Int)) $ fail "integer out of bounds"
    return $ IntLiteral offset $ fromInteger integer

floatP :: Parser Value
floatP = FloatLiteral <$> getOffset <*> signedFloatP

charP :: Parser Value
charP = CharLiteral <$> getOffset <*> charLiteralP

compilerDefinedP :: Parser Value
compilerDefinedP = Undefined <$> getOffset <*> getSourcePos <* symbol "undefined"

definedValueP :: Parser Value
definedValueP = do 
    (name, referentialType) <- lowerCaseNameWithTypeP
    return $ maybe (DefinedValue name) (DefinedValueFromInstance name . Right) referentialType

definedValueWithPrefixP :: Parser Value
definedValueWithPrefixP = do
    (name, referentialType) <- lowerCaseNameWithTypeP
    let maybeArrowOperator = snd <$> find ((== name) . fst) operatorNamePairs
    case maybeArrowOperator of
        Nothing -> return $ maybe (DefinedValue name) (DefinedValueFromInstance name . Right) referentialType
        (Just arrowOperator) -> do
            argument <- optional $ try $ valueP False
            case argument of
                Nothing -> return $ maybe (DefinedValue name) (DefinedValueFromInstance name . Right) referentialType
                (Just value) -> return $ arrowOperator (nameOffset name) referentialType value
  where
    operatorNamePairs =
        [ ("first", UnaryArrowOperator ArrowFirst)
        , ("second", UnaryArrowOperator ArrowSecond)
        , ("left", UnaryArrowOperator ArrowLeft)
        , ("right", UnaryArrowOperator ArrowRight)
        , ("const", UnaryArrowOperator ArrowConstant)
        , ("arr", UnaryArrowOperator Arr)
        , ("l", \offset _ -> SumLiteral offset False)
        , ("r", \offset _ -> SumLiteral offset True)
        ]

lowerCaseNameWithTypeP :: Parser (Name, Maybe ReferentialType)
lowerCaseNameWithTypeP = do 
    name <- lowerCaseNameP
    referentialType <- typeHintP 
    return (name, referentialType)

typeHintP :: Parser (Maybe ReferentialType)
typeHintP = optional $ between (symbol "[") (symbol "]") typeP

productP :: Parser Value
productP = parens $ ProductLiteral <$> getOffset <*> valueP True <* symbol "," <*> valueP True

listP :: Parser Value
listP = lexeme $ between (symbol "[") (symbol "]") p
  where
    p = assembleList <$> sepBy ((,) <$> getOffset <*> valueP True) (symbol ",") <*> getOffset

assembleList :: [(ParsingOffset, Value)] -> ParsingOffset -> Value
assembleList [] finalOffset = SumLiteral finalOffset False (EmptyTupleLiteral finalOffset)
assembleList ((offset, x) : xs) finalOffset = SumLiteral offset True (ProductLiteral offset x $ assembleList xs finalOffset)

stringP :: Parser Value
stringP = lexeme $ do
    offset <- getOffset
    characters <- stringLiteralP
    return $ assembleList (map ((offset,) . CharLiteral offset) characters) offset

valueTermP :: Bool -> Parser Value
valueTermP inParens =
    choice
        [ emptyTupleP
        , compilerDefinedP
        , boolP
        , try floatP
        , intP
        , charP
        , try $ if inParens then definedValueWithPrefixP else definedValueP
        , listP
        , stringP
        , try productP
        , parens $ valueP True
        ]

valueOperatorTable :: Bool -> [[Operator Parser Value]]
valueOperatorTable inParens =
    if inParens
        then
            [
                [ InfixN (BinaryArrowOperator TripleAsterisks <$> getOffset <*> (symbol "***" *> typeHintP))
                , InfixN (BinaryArrowOperator TripleAnd <$> getOffset <*> (symbol "&&&" *> typeHintP))
                , InfixN (BinaryArrowOperator TriplePlus <$> getOffset <*> (symbol "+++" *> typeHintP))
                , InfixN (BinaryArrowOperator TripleBar <$> getOffset <*> (symbol "|||" *> typeHintP))

                ]
            ,
                [ InfixR (BinaryArrowOperator ArrowComposition <$> getOffset <*> (symbol ">>>" *> typeHintP))
                , InfixR (flip <$> (BinaryArrowOperator ArrowComposition <$> getOffset <*> (symbol "<<<" *> typeHintP)))
                ]
            ]
        else []
