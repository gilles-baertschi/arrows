module Main where

import Checker.Names
import Checker.Types
import Data.Text (pack)
import Parser.Primitives
import Parser.Program
import Parser.Types
import Text.Megaparsec

main :: IO ()
main = putStrLn "Hello, Haskell!"

test :: IO ()
test = readFile "test.txt" >>= parseTest programP . pack
