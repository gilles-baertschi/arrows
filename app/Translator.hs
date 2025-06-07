{-# LANGUAGE TemplateHaskell #-}

module Translator where

import Ast
import Checker.Types
import Control.Monad.State hiding (state)
import Data.FileEmbed (embedStringFile)
import Data.List
import Data.Maybe
import Helpers
import Parser.Primitives

data TranslationState = TranslationState {stateReferences :: [Type], stateText :: String, stateDefinitions :: [TranslatedDefinition], stateDataSection :: [DataSection]}

data DataSection = FloatData Double | StringData String

data TranslatedDefinition = TranslatedDefinition {translatedName :: Name, translatedIndex :: Maybe Int, translatedType :: Maybe ReferentialType, translatedText :: Maybe String}
    deriving (Show)

translate :: ParserWithState Program String
translate =
    do
        (TranslationState _ _ translatedDefinitions dataSection) <-
            execStateT (translateDefinition "main" Nothing Nothing) (TranslationState [] "" (map snd compilerDefinitions) [])
        let dataSectionText = "section .data\n" ++ intercalate "\n" (zipWith dataSectionToText [0 ..] dataSection) ++ "\n"
        let start = "section .text\nglobal _start\n_start:\npush rbp\nmov rbp, rsp\ncall fun0\nmov rsp, rbp\npop rbp\nmov rax, 60\nxor rdi, rdi\nsyscall\n\n"
        let body =
                intercalate "\n"
                    $ map
                        ( \(index, translatedDefinition) ->
                            createFunctionName index ++ ":\npush rbp\nmov rbp, rsp\n" ++ fromJust (translatedText translatedDefinition) ++ "mov rsp, rbp\npop rbp\nret\n\n"
                        )
                    $ filter (isJust . translatedText . snd) (zip [0 ..] translatedDefinitions)
        return $ dataSectionText ++ start ++ body ++ prelude

translateDefinition :: Name -> Maybe Int -> Maybe ReferentialType -> ParserWithDoubleState TranslationState Program ()
translateDefinition name maybeIndex maybeReferentialType = do
    eitherDefinitions <- lift $ getDefinitionsFromName name
    let definition = either id (!! fromMaybe 0 maybeIndex) eitherDefinitions
    let referentialType = fromMaybe (definitionType definition) maybeReferentialType
    translationIndex <- gets $ length . stateDefinitions
    (mainTypeWithValue, mainTypeReferences) <- lift $ assertReferentialType referentialType $ definitionValue definition
    backupReferences <- gets stateReferences
    backupText <- gets stateText
    modify $ \state ->
        state
            { stateReferences = mainTypeReferences
            , stateDefinitions = stateDefinitions state ++ [TranslatedDefinition name maybeIndex (Just referentialType) Nothing]
            }
    translateValue mainTypeWithValue
    newText <- gets stateText
    modify $ \state ->
        state
            { stateReferences = backupReferences
            , stateText = backupText
            , stateDefinitions = replace translationIndex (TranslatedDefinition name maybeIndex (Just referentialType) (Just newText)) $ stateDefinitions state
            }

callDefinition :: Type -> Name -> Maybe Int -> ParserWithDoubleState TranslationState Program ()
callDefinition t name maybeIndex = do
    referentialType <- gets $ ReferentialType t . stateReferences
    maybeTranslatedDefinition <-
        gets $
            find
                ( \(_, translatedDefinition) ->
                    translatedName translatedDefinition == name
                        && translatedIndex translatedDefinition == maybeIndex
                        && maybe True (== referentialType) (translatedType translatedDefinition)
                )
                . zip [0 ..]
                . stateDefinitions
    functionIndex <- case maybeTranslatedDefinition of
        Just (i, _) -> return i
        Nothing -> do
            i <- gets $ length . stateDefinitions
            translateDefinition name maybeIndex $ Just referentialType
            return i
    write $ "call " ++ createFunctionName functionIndex ++ "\n"

translateValue :: TypeWithValue -> ParserWithDoubleState TranslationState Program ()
translateValue (TypeWithIntLiteral _ value) = write $ "mov rax, " ++ show value ++ "\n"
translateValue (TypeWithCharLiteral _ value) = write $ "mov rax, " ++ show value ++ "\n"
translateValue (TypeWithBoolLiteral _ value) = write $ "mov rax, " ++ if value then "1" else "0" ++ "\n"
translateValue (TypeWithFloatLiteral _ value) = do
    maybeIndex <-
        gets $
            fmap fst
                . find
                    ( \(_, dataSection) -> case dataSection of
                        (FloatData value') -> value == value'
                        _ -> False
                    )
                . zip [0 ..]
                . stateDataSection
    name <-
        createConstantName <$> case maybeIndex of
            (Just i) -> return i
            Nothing -> addFloatSection value
    write $ "mov rax, " ++ name
translateValue (TypeWithDefinedValue t name) = callDefinition t name Nothing
-- referentialType <- gets $ ReferentialType t . stateReferences
-- maybeTranslatedDefinition <-
--     gets $
--         find
--             ( \(_, translatedDefinition) ->
--                 translatedName translatedDefinition == name
--                     && isNothing (translatedIndex translatedDefinition)
--                     && maybe True (== referentialType) (translatedType translatedDefinition)
--             )
--             . zip [0 ..]
--             . stateDefinitions
-- functionIndex <- case maybeTranslatedDefinition of
--     Just (i, _) -> return i
--     Nothing -> do
--         i <- gets $ length . stateDefinitions
--         translateDefinition name Nothing $ Just referentialType
--         return i
-- write $ "call " ++ createFunctionName functionIndex ++ "\n"
translateValue (TypeWithDefinedValueFromInstance t name index) = callDefinition t name $ Just index
-- referentialType <- gets $ ReferentialType t . stateReferences
-- maybeTranslatedDefinition <-
--     gets $
--         find
--             ( \(_, translatedDefinition) ->
--                 translatedName translatedDefinition == name
--                     && translatedIndex translatedDefinition == Just index
--                     && maybe True (== referentialType) (translatedType translatedDefinition)
--             )
--             . zip [0 ..]
--             . stateDefinitions
-- functionIndex <- case maybeTranslatedDefinition of
--     Just (i, _) -> return i
--     Nothing -> do
--         i <- gets $ length . stateDefinitions
--         translateDefinition name (Just index) $ Just referentialType
--         return i
-- write $ "call " ++ createFunctionName functionIndex ++ "\n"
translateValue (TypeWithArrowConstant _ value) = translateValue value
translateValue (TypeWithArrowComposition _ x y) = do
    translateValue x
    translateValue y
translateValue (TypeWithUndefined _) = return ()
translateValue _ = undefined

write :: String -> ParserWithDoubleState TranslationState Program ()
write newText = modify $ \state -> state{stateText = stateText state ++ newText}

addFloatSection :: Double -> ParserWithDoubleState TranslationState Program Int
addFloatSection value = do
    index <- gets $ length . stateDataSection
    modify $ \state -> state{stateDataSection = stateDataSection state ++ [FloatData value]}
    return index

dataSectionToText :: Int -> DataSection -> String
dataSectionToText index (FloatData value) = createConstantName index ++ " dq " ++ show value
dataSectionToText index (StringData value) = createConstantName index ++ " db " ++ show value ++ ", 0"

compilerDefinitions :: [(String, TranslatedDefinition)]
compilerDefinitions =
    [ ("put_char", TranslatedDefinition "putChar" Nothing Nothing Nothing)
    ]

createFunctionName :: Int -> String
createFunctionName index = if index < length compilerDefinitions then fst $ compilerDefinitions !! index else "fun" ++ show (index - length compilerDefinitions)

createConstantName :: Int -> String
createConstantName index = "const" ++ show index

prelude :: String
prelude = $(embedStringFile "Prelude.asm")

-- = ProductLiteral ParsingOffset Value Value
-- \| SumLiteral ParsingOffset Bool Value
-- \| EmptyTupleLiteral ParsingOffset
-- \| ArrowFirst ParsingOffset Value
-- \| ArrowSecond ParsingOffset Value
-- \| TripleAsterisks ParsingOffset Value Value
-- \| TripleAnd ParsingOffset Value Value
-- \| ArrowRight ParsingOffset Value
-- \| ArrowLeft ParsingOffset Value
-- \| TriplePlus ParsingOffset Value Value
-- \| TripleBar ParsingOffset Value Value
