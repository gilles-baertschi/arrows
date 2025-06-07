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
import Text.Megaparsec.Char (char)
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

sumLiteralP :: Parser Value
sumLiteralP = do
    offset <- getOffset
    (boolChoice, value) <- left <|> right
    return $ SumLiteral offset boolChoice value
  where
    left = between (char '(') (char '|') ((False,) <$> valueP)
    right = between (char '|') (char ')') ((True,) <$> valueP)

valueTermP :: Parser Value
valueTermP =
    choice
        [ emptyTupleP
        , compilerDefinedP
        , boolP
        , try floatP
        , intP
        , charP
        , try definedValueP
        , try sumLiteralP
        , parens valueP
        ]

valueOperatorTable :: [[Operator Parser Value]]
valueOperatorTable =
    [ [binaryL "," (ProductLiteral <$> getOffset)]
    , -- , [binaryL "|" (SumLiteral <$> getOffset)]

        [ prefix "first" (ArrowFirst <$> getOffset)
        , prefix "second" (ArrowSecond <$> getOffset)
        , prefix "left" (ArrowLeft <$> getOffset)
        , prefix "right" (ArrowRight <$> getOffset)
        , prefix "const" (ArrowConstant <$> getOffset)
        ]
    ,
        [ binaryN "***" (TripleAsterisks <$> getOffset)
        , binaryN "&&&" (TripleAnd <$> getOffset)
        , binaryN "+++" (TriplePlus <$> getOffset)
        , binaryN "|||" (TripleBar <$> getOffset)
        ]
    ,
        [ binaryR ">>>" (ArrowComposition <$> getOffset)
        , binaryR "<<<" (flip . ArrowComposition <$> getOffset)
        ]
    ]
