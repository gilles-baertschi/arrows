{-# LANGUAGE TupleSections #-}

module Parser.Values (valueP, instanceP, definitionP) where

import Ast
import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Functor
import Data.Text (pack)
import Parser.Primitives
import Parser.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

definitionP :: Parser Definition
definitionP = nonIndented p
  where
    p = do
        name <- lowerCaseNameP
        _ <- symbol "::"
        referentialType <- typeP
        _ <- newLineP
        _ <- symbol $ pack $ nameString name
        _ <- symbol "="
        value <- valueP
        _ <- newLineP
        return $ Definition name referentialType value

instanceP :: Parser Instance
instanceP = nonIndented (L.indentBlock newLineP p)
  where
    member = do
        memberName <- lowerCaseNameP
        _ <- symbol "="
        memberValue <- valueP
        return (memberName, memberValue)
    p = do
        _ <- symbol "instance"
        constraints <- try typeConstraintsP <|> return []
        nameOfClass <- upperCaseNameP
        instancedType <- typeWithExistingConstraintsP constraints
        _ <- "where"
        return $ L.IndentSome Nothing (return . Instance instancedType nameOfClass) member

valueP :: Parser Value
valueP = makeExprParser valueTermP valueOperatorTable

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
compilerDefinedP = Undefined <$> getOffset <* symbol "undefined"

definedValueP :: Parser Value
definedValueP = DefinedValue <$> lowerCaseNameP

productP :: Parser Value
productP = parens $ ProductLiteral <$> getOffset <*> valueP <* symbol "," <*> valueP

listP :: Parser Value
listP = lexeme $ between (symbol "[") (symbol "]") p
  where
    p = assembleList <$> sepBy ((,) <$> getOffset <*> valueP) (symbol ",") <*> getOffset

assembleList :: [(ParsingOffset, Value)] -> ParsingOffset -> Value
assembleList [] finalOffset = SumLiteral finalOffset False (EmptyTupleLiteral finalOffset)
assembleList ((offset, x) : xs) finalOffset = SumLiteral offset True (ProductLiteral offset x $ assembleList xs finalOffset)

stringP :: Parser Value
stringP = lexeme $ do
    offset <- getOffset
    characters <- stringLiteralP
    return $ assembleList (map ((offset,) . CharLiteral offset) characters) offset

-- unaryArrowOperatorP :: Parser Value
-- unaryArrowOperatorP = UnaryArrowOperator <$> operatorP <*> getOffset <*> operandP
--   where
--     operatorP =
--         choice
--             [ symbol "const" $> ArrowConstant
--             , symbol "arr" $> Arr
--             , symbol "first" $> ArrowFirst
--             , symbol "second" $> ArrowSecond
--             , symbol "left" $> ArrowLeft
--             , symbol "right" $> ArrowRight
--             ]
--     operandP =
--         choice
--             [ emptyTupleP
--             , compilerDefinedP
--             , boolP
--             , try floatP
--             , intP
--             , charP
--             , try definedValueP
--             , try sumLiteralP
--             , parens valueP
--             ]

-- sumLiteralP :: Parser Value
-- sumLiteralP = do
--     offset <- getOffset
--     (boolChoice, value) <- left <|> right
--     return $ SumLiteral offset boolChoice value
--   where
--     left = between (char '(') (char '|') ((False,) <$> valueP)
--     right = between (char '|') (char ')') ((True,) <$> valueP)

valueTermP :: Parser Value
valueTermP =
    choice
        [ emptyTupleP
        , compilerDefinedP
        , boolP
        , try floatP
        , intP
        , try charP
        , try definedValueP
        , listP
        , stringP
        , try productP
        , parens valueP
        ]

valueOperatorTable :: [[Operator Parser Value]]
valueOperatorTable =
    [
        [ prefix "first" (UnaryArrowOperator ArrowFirst <$> getOffset)
        , prefix "second" (UnaryArrowOperator ArrowSecond <$> getOffset)
        , prefix "left" (UnaryArrowOperator ArrowLeft <$> getOffset)
        , prefix "right" (UnaryArrowOperator ArrowRight <$> getOffset)
        , prefix "const" (UnaryArrowOperator ArrowConstant <$> getOffset)
        , prefix "arr" (UnaryArrowOperator Arr <$> getOffset)
        , prefix "l" (flip SumLiteral False <$> getOffset)
        , prefix "r" (flip SumLiteral True <$> getOffset)
        ]
    ,
        [ binaryN "***" (BinaryArrowOperator TripleAsterisks <$> getOffset)
        , binaryN "&&&" (BinaryArrowOperator TripleAnd <$> getOffset)
        , binaryN "+++" (BinaryArrowOperator TriplePlus <$> getOffset)
        , binaryN "|||" (BinaryArrowOperator TripleBar <$> getOffset)
        ]
    ,
        [ binaryR ">>>" (BinaryArrowOperator ArrowComposition <$> getOffset)
        , binaryR "<<<" (flip . BinaryArrowOperator ArrowComposition <$> getOffset)
        ]
        -- , [binaryN "," (ProductLiteral <$> getOffset)]
    ]
