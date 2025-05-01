module Main where

import Checker.Names
import Checker.Types
import Data.Text
import qualified Data.Text.IO as T
import Parser.Primitives
import Parser.Program
import Parser.Types
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Text.Megaparsec

main :: IO ()
main = return ()

test :: IO ()
test = do
    let fileName = "test.txt"
    input <- pack <$> readFile fileName
    let result = runParser programP fileName input
    case result of
        Left parserErrorBundel -> putStrLn $ errorBundlePretty parserErrorBundel
        Right program -> do
            let doc = pretty $ show program
            T.putStrLn (renderStrict (layoutPretty defaultLayoutOptions doc))
