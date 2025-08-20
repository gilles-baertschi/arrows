{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Console.GetOpt
import Checker.Names
import Checker.Types
import Data.FileEmbed (embedStringFile)
import Data.Text (replace, unpack, pack)
import qualified Data.Text.IO as T
import Parser.Primitives
import Parser.Program
import Parser.Types
import System.Process
import Translator
import Text.Megaparsec
import Data.List
import Data.List.Split
import System.IO
import System.Process
import System.Environment
import System.Exit
import Control.Monad

usage :: String -> String
usage name = unpack $ replace "kpyc" (pack name) $ pack $(embedStringFile "usage.txt")

data Options = Options { optExecute :: Bool, optDebug :: Bool, optOutput :: String }

defaultOptions :: Options
defaultOptions = Options { optExecute = False, optDebug = False, optOutput = "" }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "o" ["output"] (ReqArg (\arg opt -> return opt { optOutput = arg }) "FILE") "Specify the name of the final executable"
    , Option "e" ["execute"] (NoArg (\opt -> return opt { optExecute = True })) "Execute after compilation"
    , Option "d" ["debug"] (NoArg (\opt -> return opt { optDebug = True })) "Add debug symbols"
    , Option "h" ["help"] (NoArg (\_ -> do name <- getProgName; hPutStrLn stderr $ usage name; exitSuccess)) "Show help"
    ]

main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOptions, _) = getOpt Permute options args
    Options { optExecute = execute, optOutput = output, optDebug = debug } <- foldl (>>=) (return defaultOptions) actions
    codeFileName <- case nonOptions of
        [filePath] -> return filePath
        _ -> do
            hPutStrLn stderr $ "Error: A single file argument is required. " ++ show nonOptions
            exitFailure
    let outputFileName = if output == "" then intercalate "." . init $ splitOn "." codeFileName else output
    code <- readFile codeFileName
    success <- compile debug code codeFileName outputFileName
    when (execute && success) $ callProcess ("./" ++ outputFileName) []

run :: IO ()
run = withArgs ["test.txt", "-ed"] main

compile :: Bool -> String -> String -> String -> IO Bool
compile debug code codeFileName outputFileName = do
    let result = runParser programP codeFileName $ pack $ code ++ prelude
    case result of
        Left parserErrorBundel -> do 
            putStrLn $ errorBundlePretty parserErrorBundel 
            return False
        Right program -> do
            writeFile (outputFileName ++ ".asm") program
            let nasmArgs = if debug then ["-f", "elf64", "-F", "dwarf", outputFileName ++ ".asm"] else ["-f", "elf64", outputFileName ++ ".asm"]
            callProcess "nasm" nasmArgs
            callProcess "ld" [outputFileName ++ ".o", "-o", outputFileName]
            return True

prelude :: String
prelude = $(embedStringFile "app/Prelude/Prelude.txt")
