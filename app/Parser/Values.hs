module Parser.Values (valueP, instanceP) where

import Ast
import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Functor
import Parser.Primitives
import Parser.Types
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

instanceP :: Parser Instance
instanceP = L.nonIndented newLineP (L.indentBlock newLineP p)
  where
    member = do
        -- _ <- newLineP
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
        -- _ <- newLineP
        -- members <- some member
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
compilerDefinedP = CompilerDefined <$> getOffset <* symbol "undefined"

definedValueP :: Parser Value
definedValueP = DefinedValue <$> getOffset <*> lowerCaseNameP

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
        , parens valueP
        ]

valueOperatorTable :: [[Operator Parser Value]]
valueOperatorTable =
    [ [binaryL "," (ProductLiteral <$> getOffset)]
    , [binaryL "|" (SumLiteral <$> getOffset)]
    ,
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
