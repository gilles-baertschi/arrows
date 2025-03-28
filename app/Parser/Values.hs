module Parser.Values where

import Ast
import Control.Monad.Combinators.Expr
import Data.Functor
import Parser.Primitives
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

valueP :: Parser Value
valueP = makeExprParser valueTermP valueOperatorTable

emptyTupleP :: Parser Value
emptyTupleP = symbol "()" $> EmptyTupleLiteral

boolP :: Parser Value
boolP = symbol "True" $> BoolLiteral True <|> symbol "False" $> BoolLiteral False

intP :: Parser Value
intP = IntLiteral <$> lexeme L.decimal

floatP :: Parser Value
floatP = FloatLiteral <$> lexeme L.float

charP :: Parser Value
charP = between (char '\'') (char '\'') $ CharLiteral <$> printChar

compilerDefinedP :: Parser Value
compilerDefinedP = symbol "undefined" $> CompilerDefined

aliasInstanceP :: Parser Value
aliasInstanceP = AliasInstance <$> upperCaseNameP <*> many valueP

definedValueP :: Parser Value
definedValueP = DefinedValue <$> lowerCaseNameP

valueTermP :: Parser Value
valueTermP =
    choice
        [ emptyTupleP
        , compilerDefinedP
        , boolP
        , try floatP
        , intP
        , charP
        , try aliasInstanceP
        , try definedValueP
        , parens valueP
        ]

valueOperatorTable :: [[Operator Parser Value]]
valueOperatorTable =
    [ [binary "," ProductLiteral]
    , [binary "|" SumLiteral]
    ]
