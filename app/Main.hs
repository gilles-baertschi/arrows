{-# LANGUAGE TemplateHaskell #-}

module Main where

import Ast
import Checker.Names
import Checker.Types
import Control.Monad
import Control.Monad.State
import Data.FileEmbed (embedStringFile)
import Data.List
import Data.List.Split
import Data.Text (Text, pack, replace, unpack)
import Data.Void
import Parser.Primitives
import Parser.Program
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Megaparsec
import qualified Translator.Nasm as Nasm
import qualified Translator.Llvm as Llvm

usage :: String -> String
usage name = unpack $ replace "pirat" (pack name) $ pack $(embedStringFile "usage.txt")

data Options = Options {optExecute :: Bool, optDebug :: Bool, optLLVM :: Bool, optOutput :: String}

defaultOptions :: Options
defaultOptions = Options {optExecute = False, optDebug = False, optLLVM = False, optOutput = ""}

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "o" ["output"] (ReqArg (\arg opt -> return opt {optOutput = arg}) "FILE") "Specify the name of the final executable",
    Option "e" ["execute"] (NoArg (\opt -> return opt {optExecute = True})) "Execute after compilation",
    Option "d" ["debug"] (NoArg (\opt -> return opt {optDebug = True})) "Add debug symbols",
    Option "h" ["help"] (NoArg (\_ -> do name <- getProgName; hPutStrLn stderr $ usage name; exitSuccess)) "Show help",
    Option "l" ["llvm"] (NoArg (\opt -> return opt {optLLVM = True})) "Use LLVM"
  ]

main :: IO ()
main = do
  args <- getArgs
  let (actions, nonOptions, _) = getOpt Permute options args
  Options {optExecute = execute, optOutput = output, optLLVM = llvm, optDebug = debug} <- foldl (>>=) (return defaultOptions) actions
  codeFileName <- case nonOptions of
    [filePath] -> return filePath
    _ -> do
      hPutStrLn stderr $ "Error: A single file argument is required. " ++ show nonOptions
      exitFailure
  let outputFileName = if output == "" then intercalate "." . init $ splitOn "." codeFileName else output
  code <- readFile codeFileName
  success <- compileAssembleLinkRun llvm debug code codeFileName outputFileName
  when (execute && success) $ callProcess ("./" ++ outputFileName) []

run :: IO ()
run = withArgs ["test.txt", "-ed"] main

compileAssembleLinkRun :: Bool -> Bool -> String -> String -> String -> IO Bool
compileAssembleLinkRun False debug code codeFileName outputFileName = do
  let result = compileNasm codeFileName $ pack $ code ++ prelude
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
compileAssembleLinkRun True debug code codeFileName outputFileName = do
  let result = compileLlvm codeFileName $ pack $ code ++ prelude
  case result of
    Left parserErrorBundel -> do
      putStrLn $ errorBundlePretty parserErrorBundel
      return False
    Right program -> do
      writeFile (outputFileName ++ ".ll") program
      callProcess "clang" [outputFileName ++ ".ll", "-o", outputFileName]
      return True

compileLlvm :: String -> Text -> Either (ParseErrorBundle Text Void) String
compileLlvm = runParser $ programP >>= evalStateT (checkAll >> Llvm.translate)

compileNasm :: String -> Text -> Either (ParseErrorBundle Text Void) String
compileNasm = runParser $ programP >>= evalStateT (checkAll >> Nasm.translate)

checkAll :: ParserWithState Program ()
checkAll = checkNameSafety >> checkTypeSafety

prelude :: String
prelude = $(embedStringFile "app/Prelude/Prelude.txt")
