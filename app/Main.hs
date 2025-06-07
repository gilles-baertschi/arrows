module Main where

import Checker.Names
import Checker.Types
import Data.Text
import qualified Data.Text.IO as T
import Parser.Primitives
import Parser.Program
import Parser.Types
import System.Process
import Translator

import Text.Megaparsec

main :: IO ()
main = return ()

test :: IO ()
test = do
    let fileName = name ++ ".txt"
    input <- pack <$> readFile fileName
    let result = runParser onlyProgramP fileName input
    case result of
        Left parserErrorBundel -> putStrLn $ errorBundlePretty parserErrorBundel
        Right program -> do
            -- let doc = pretty $ show program
            -- T.putStrLn (renderStrict (layoutPretty defaultLayoutOptions doc))
            print program

trans :: IO ()
trans = do
    let fileName = name ++ ".txt"
    input <- pack <$> readFile fileName
    let result = runParser programP fileName input
    case result of
        Left parserErrorBundel -> putStrLn $ errorBundlePretty parserErrorBundel
        Right program -> do
            putStrLn program

run :: IO ()
run = do
    let fileName = name ++ ".txt"
    input <- pack <$> readFile fileName
    let result = runParser programP fileName input
    case result of
        Left parserErrorBundel -> putStrLn $ errorBundlePretty parserErrorBundel
        Right program -> do
            writeFile (name ++ ".asm") program
            let nasmArgs = if debug then ["-f", "elf64", "-F", "dwarf", name ++ ".asm"] else ["-f", "elf64", name ++ ".asm"]
            callProcess "nasm" nasmArgs
            callProcess "ld" [name ++ ".o", "-o", name]
            callProcess ("./" ++ name) []

name :: String
name = "test"

debug :: Bool
debug = True
