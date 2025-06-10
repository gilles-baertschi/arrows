{-# LANGUAGE TemplateHaskell #-}

module Translator where

import Ast
import Checker.Types
import Control.Monad.State hiding (state)
import Data.Char (ord)
import Data.FileEmbed (embedStringFile)
import Data.Functor
import Data.List
import Data.Maybe
import Helpers
import Parser.Primitives

data TranslationState = TranslationState {stateReferences :: [Type], stateText :: String, stateDefinitions :: [TranslatedDefinition], stateDataSection :: [DataSection], stateLabelIndex :: Int}

data DataSection = FloatData Double | StringData String

data TranslatedDefinition = TranslatedDefinition {translatedName :: Maybe Name, translatedIndex :: Maybe Int, translatedType :: Maybe ReferentialType, translatedText :: Maybe String}
    deriving (Show, Read, Eq, Ord)

data ReturnType = ReturnValueInRax | ReturnFunctionName String

translate :: ParserWithState Program String
translate =
    do
        (TranslationState _ _ translatedDefinitions dataSection _) <-
            execStateT
                ( do
                    translateDefinition "main" Nothing Nothing
                )
                (TranslationState [] "" (map snd compilerDefinitions) [] 0)
        let dataSectionText = "section .data\n" ++ intercalate "\n" (zipWith dataSectionToText [0 ..] dataSection) ++ "\n"
        let start = "section .text\nglobal _start\n_start:\npush rbp\nmov rbp, rsp\ncall fun0\nmov rsp, rbp\npop rbp\nmov rax, 60\nxor rdi, rdi\nsyscall\n\n"
        let body =
                concatMap
                    ( \(index, translatedDefinition) ->
                        createFunctionName index ++ ":\npush rbp\nmov rbp, rsp\n" ++ fromJust (translatedText translatedDefinition) ++ "mov rsp, rbp\npop rbp\nret\n\n"
                    )
                    $ filter (isJust . translatedText . snd) (zip [0 ..] translatedDefinitions)
        return $ dataSectionText ++ start ++ body ++ preludeAssembly

translateDefinition :: Name -> Maybe Int -> Maybe ReferentialType -> ParserWithDoubleState TranslationState Program String
translateDefinition name maybeIndex maybeReferentialType = do
    eitherDefinitions <- lift $ getDefinitionsFromName name
    let definition = either id (!! fromMaybe 0 maybeIndex) eitherDefinitions
    let referentialType = fromMaybe (definitionType definition) maybeReferentialType
    (mainTypeWithValue, mainTypeReferences) <- lift $ assertReferentialType referentialType $ definitionValue definition
    translateInContext
        ( do
            modify $ \state ->
                state
                    { stateReferences = mainTypeReferences
                    , stateDefinitions = stateDefinitions state ++ [TranslatedDefinition (Just name) maybeIndex (Just referentialType) Nothing]
                    }
            translateValue mainTypeWithValue >>= callReturn
        )

translateInContext :: ParserWithDoubleState TranslationState Program () -> ParserWithDoubleState TranslationState Program String
translateInContext action = do
    translationIndex <- gets $ length . stateDefinitions
    backupReferences <- gets stateReferences
    backupText <- gets stateText
    modify $ \state -> state{stateText = ""}
    action
    newText <- gets stateText
    modify $ \state ->
        state
            { stateReferences = backupReferences
            , stateText = backupText
            , stateDefinitions =
                let (TranslatedDefinition maybeName maybeIndex maybeReferentialType _) = stateDefinitions state !! translationIndex
                 in replace translationIndex (TranslatedDefinition maybeName maybeIndex maybeReferentialType (Just newText)) $ stateDefinitions state
            }
    return $ createFunctionName translationIndex

translateAnonymous :: ParserWithDoubleState TranslationState Program () -> ParserWithDoubleState TranslationState Program ReturnType
translateAnonymous action = do
    function <-
        translateInContext
            ( do
                modify $ \state ->
                    state
                        { stateDefinitions = stateDefinitions state ++ [TranslatedDefinition Nothing Nothing Nothing Nothing]
                        }
                action
            )
    return $ ReturnFunctionName function

-- loadAnonymousValue :: TypeWithValue -> ParserWithDoubleState TranslationState Program ()
-- loadAnonymousValue value = translateInAnonymousContext $ translateValue value

loadDefinition :: Type -> Name -> Maybe Int -> ParserWithDoubleState TranslationState Program String
loadDefinition t name maybeIndex = do
    referentialType <- gets $ ReferentialType t . stateReferences
    maybeTranslatedDefinition <-
        gets $
            find
                ( \(_, translatedDefinition) ->
                    translatedName translatedDefinition == Just name
                        && maybe
                            True
                            ( \alreadyTranslatedType ->
                                translatedIndex translatedDefinition == maybeIndex
                                    && referentialType == alreadyTranslatedType
                            )
                            (translatedType translatedDefinition)
                )
                . zip [0 ..]
                . stateDefinitions
    case maybeTranslatedDefinition of
        Just (i, _) -> return $ createFunctionName i
        Nothing -> translateDefinition name maybeIndex $ Just referentialType

translateValue :: TypeWithValue -> ParserWithDoubleState TranslationState Program ReturnType
translateValue (TypeWithIntLiteral _ value) = write ("mov rax, " ++ show value ++ "\n") $> ReturnValueInRax
translateValue (TypeWithCharLiteral _ value) = write ("mov rax, " ++ show (ord value) ++ "\n") $> ReturnValueInRax
translateValue (TypeWithBoolLiteral _ value) = write ("mov rax, " ++ (if value then "1" else "0") ++ "\n") $> ReturnValueInRax
translateValue (TypeWithEmptyTupleLiteral _) = return ReturnValueInRax
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
    write ("mov rax, " ++ name ++ "\n")
    return ReturnValueInRax
translateValue (TypeWithProductLiteral _ x y) = do
    write "mov rax, 16\ncall alloc\npush rax\n"
    translateValue x >>= returnInRax
    write "mov rdi, [rsp]\nmov [rdi], rax\n"
    translateValue y >>= returnInRax
    write "mov rdi, [rsp]\nmov [rdi+8], rax\npop rax\n"
    return ReturnValueInRax
translateValue (TypeWithSumLiteral _ boolChoice value) = do
    write "mov rax, 16\ncall alloc\npush rax\n"
    write $ "mov qword [rax], " ++ (if boolChoice then "1" else "0") ++ "\n"
    translateValue value >>= returnInRax
    write "mov rdi, [rsp]\nmov [rdi+8], rax\npop rax\n"
    return ReturnValueInRax
translateValue (TypeWithDefinedValue t name) = ReturnFunctionName <$> loadDefinition t name Nothing
translateValue (TypeWithDefinedValueFromInstance t name index) = ReturnFunctionName <$> loadDefinition t name (Just index)
translateValue (TypeWithUnaryArrowOperator Arr _ value) = translateValue value
translateValue (TypeWithUnaryArrowOperator ArrowConstant _ value) = translateAnonymous $ translateValue value >>= returnInRax
translateValue (TypeWithBinaryArrowOperator ArrowComposition _ x y) =
    translateAnonymous
        ( do
            translateValue x >>= callReturn
            -- write "call rax\npush rax\n"
            translateValue y >>= callReturn
            -- write "mov rdi, rax\npop rax\ncall rdi\n"
        )
translateValue (TypeWithUnaryArrowOperator ArrowFirst _ value) =
    translateAnonymous
        ( do
            -- write "push rax\n"
            write "push rax\nmov rax, [rax]"
            translateValue value >>= callReturn
            -- write "mov rdi, rax\nmov rax, [rsp]\nmov rax, [rax]\ncall rdi\npop rdx\nmov [rdx], rax\nmov rax, rdx\n"
            write "pop rdx\nmov [rdx], rax\nmov rax, rdx\n"
        )
translateValue (TypeWithUnaryArrowOperator ArrowSecond _ value) =
    translateAnonymous
        ( do
            -- write "push rax\n"
            write "push rax\nmov rax, [rax+8]"
            translateValue value >>= callReturn
            -- write "mov rdi, rax\nmov rax, [rsp]\nmov rax, [rax+8]\ncall rdi\npop rdx\nmov [rdx+8], rax\nmov rax, rdx\n"
            write "pop rdx\nmov [rdx+8], rax\nmov rax, rdx\n"
        )
translateValue (TypeWithBinaryArrowOperator TripleAsterisks _ x y) =
    translateAnonymous
        ( do
            -- write "push rax\n"
            write "push rax\nmov rax, [rax]\n"
            translateValue x >>= callReturn
            -- write "mov rdi, rax\nmov rax, [rsp]\nmov rax, [rax]\ncall rdi\nmov rdx, [rsp]\nmov [rdx], rax\n"
            write "mov rdx, [rsp]\nmov [rdx], rax\nmov rax, [rdx+8]\n"
            translateValue y >>= callReturn
            -- write "mov rdi, rax\nmov rax, [rdx+8]\ncall rdi\npop rdx\nmov [rdx+8], rax\nmov rax, rdx\n"
            write "pop rdx\nmov [rdx+8], rax\nmov rax, rdx\n"
        )
translateValue (TypeWithBinaryArrowOperator TripleAnd t x y) =
    translateAnonymous
        ( do
            write "call double\n"
            translateValue (TypeWithBinaryArrowOperator TripleAsterisks t x y) >>= callReturn
        )
translateValue (TypeWithUnaryArrowOperator ArrowLeft _ value) =
    translateAnonymous
        ( do
            lable <- (".done_left" ++) . show <$> getNewLableIndex
            -- write $ "mov rcx, [rax]\ntest rcx, rcx\njnz " ++ lable ++ "\npush rax\n"
            write $ "mov rcx, [rax]\ntest rcx, rcx\njnz " ++ lable ++ "\npush rax\nmov rax, [rax+8]\n"
            translateValue value >>= callReturn
            -- write $ "mov rdi, rax\nmov rax, [rsp]\nmov rax, [rax+8]\ncall rdi\npop rdx\nmov [rdx+8], rax\nmov rax, rdx\n" ++ lable ++ ":\n"
            write $ "pop rdx\nmov [rdx+8], rax\nmov rax, rdx\n" ++ lable ++ ":\n"
        )
translateValue (TypeWithUnaryArrowOperator ArrowRight _ value) =
    translateAnonymous
        ( do
            lable <- (".done_right" ++) . show <$> getNewLableIndex
            -- write $ "mov rcx, [rax]\ntest rcx, rcx\njz " ++ lable ++ "\npush rax\n"
            write $ "mov rcx, [rax]\ntest rcx, rcx\njz " ++ lable ++ "\npush rax\nmov rax, [rax+8]\n"
            translateValue value >>= callReturn
            -- write $ "mov rdi, rax\nmov rax, [rsp]\nmov rax, [rax+8]\ncall rdi\npop rdx\nmov [rdx+8], rax\nmov rax, rdx\n" ++ lable ++ ":\n"
            write $ "pop rdx\nmov [rdx+8], rax\nmov rax, rdx\n" ++ lable ++ ":\n"
        )
translateValue (TypeWithBinaryArrowOperator TriplePlus _ x y) =
    translateAnonymous
        ( do
            lableIndex <- getNewLableIndex
            let caseLable = ".case_plus" ++ show lableIndex
            let doneLable = ".done_plus" ++ show lableIndex
            -- write $ "push rax\nmov rcx, [rax]\nmov rax, [rax+8]\ntest rcx, rcx\njnz " ++ caseLable ++ "\n"
            write $ "push rax\nmov rcx, [rax]\nmov rax, [rax+8]\ntest rcx, rcx\njnz " ++ caseLable ++ "\n"
            translateValue x >>= callReturn
            -- write $ "call rdi\njmp " ++ doneLable ++ "\n" ++ caseLable ++ ":\n"
            write $ "jmp " ++ doneLable ++ "\n" ++ caseLable ++ ":\n"
            translateValue y >>= callReturn
            -- write $ "call rdi\n" ++ doneLable ++ ":\npop rdi\nmov [rdi+8], rax\nmov rax, rdi\n"
            write $ doneLable ++ ":\npop rdi\nmov [rdi+8], rax\nmov rax, rdi\n"
        )
translateValue (TypeWithBinaryArrowOperator TripleBar _ x y) =
    translateAnonymous
        ( do
            lableIndex <- getNewLableIndex
            let caseLable = ".case_bar" ++ show lableIndex
            let doneLable = ".done_bar" ++ show lableIndex
            -- write $ "mov rcx, [rax]\nmov rax, [rax+8]\ntest rcx, rcx\njnz " ++ caseLable ++ "\n"
            write $ "mov rcx, [rax]\nmov rax, [rax+8]\ntest rcx, rcx\njnz " ++ caseLable ++ "\n"
            translateValue x >>= callReturn
            -- write $ "call rdi\njmp " ++ doneLable ++ "\n" ++ caseLable ++ ":\n"
            write $ "jmp " ++ doneLable ++ "\n" ++ caseLable ++ ":\n"
            translateValue y >>= callReturn
            -- write $ "call rdi\n" ++ doneLable ++ ":\n"
            write $ doneLable ++ ":\n"
        )
translateValue (TypeWithUndefined _) = return ReturnValueInRax -- TODO: throw error

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

returnInRax :: ReturnType -> ParserWithDoubleState TranslationState Program ()
returnInRax (ReturnFunctionName name) = write $ "lea rax, " ++ name ++ "\n"
returnInRax ReturnValueInRax = return ()

callReturn :: ReturnType -> ParserWithDoubleState TranslationState Program ()
callReturn (ReturnFunctionName name) = write $ "call " ++ name ++ "\n"
callReturn ReturnValueInRax = undefined

compilerDefinitions :: [(String, TranslatedDefinition)]
compilerDefinitions =
    [ ("put_char", TranslatedDefinition (Just "putChar") Nothing Nothing Nothing)
    , ("read_char", TranslatedDefinition (Just "readChar") Nothing Nothing Nothing)
    , ("fst", TranslatedDefinition (Just "fst") Nothing Nothing Nothing)
    , ("snd", TranslatedDefinition (Just "snd") Nothing Nothing Nothing)
    , ("double", TranslatedDefinition (Just "double") Nothing Nothing Nothing)
    , ("id", TranslatedDefinition (Just "id") Nothing Nothing Nothing)
    , ("app", TranslatedDefinition (Just "app") Nothing Nothing Nothing)
    , ("add", TranslatedDefinition (Just "add") Nothing Nothing Nothing)
    , ("sub", TranslatedDefinition (Just "sub") Nothing Nothing Nothing)
    , ("mul", TranslatedDefinition (Just "mul") Nothing Nothing Nothing)
    , ("div", TranslatedDefinition (Just "div") Nothing Nothing Nothing)
    , ("not", TranslatedDefinition (Just "not") Nothing Nothing Nothing)
    , ("and", TranslatedDefinition (Just "and") Nothing Nothing Nothing)
    , ("or", TranslatedDefinition (Just "or") Nothing Nothing Nothing)
    ]

createFunctionName :: Int -> String
createFunctionName index = if index < length compilerDefinitions then fst $ compilerDefinitions !! index else "fun" ++ show (index - length compilerDefinitions)

createConstantName :: Int -> String
createConstantName index = "const" ++ show index

getNewLableIndex :: ParserWithDoubleState TranslationState Program Int
getNewLableIndex = do
    index <- gets stateLabelIndex
    modify $ \state -> state{stateLabelIndex = index + 1}
    return index

preludeAssembly :: String
preludeAssembly = $(embedStringFile "app/Prelude/Prelude.asm")
