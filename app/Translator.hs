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
    deriving (Show, Eq, Ord)

data ReturnType = ReturnValueInRax | ReturnFunctionName String

translate :: ParserWithState Program String
translate =
    do
        compilerDefinitionsAsTranslated <-
            mapM
                ( ( \(maybeClassName, name) -> case maybeClassName of
                        (Just className) -> TranslatedDefinition (Just name) . Just <$> getIndexFromNameAndClass className name <*> return Nothing <*> return Nothing
                        Nothing -> return $ TranslatedDefinition (Just name) Nothing Nothing Nothing
                  )
                    . snd
                )
                compilerDefinitions
        (TranslationState _ _ translatedDefinitions dataSection _) <-
            execStateT
                ( do
                    translateDefinition "main" Nothing Nothing
                )
                (TranslationState [] "" compilerDefinitionsAsTranslated [] 0)
        let dataSectionText = "section .data\n" ++ intercalate "\n" (zipWith dataSectionToText [0 ..] dataSection) ++ "\n"
        let start = "section .text\nglobal _start\n_start:\npush rbp\nmov rbp, rsp\ncall fun0\nmov rsp, rbp\npop rbp\nmov rax, 60\nxor rdi, rdi\nsyscall\n\n"
        let body =
                concatMap
                    ( \(index, translatedDefinition) ->
                        createFunctionName index ++ ":\npush rbp\nmov rbp, rsp\n" ++ fromJust (translatedText translatedDefinition) ++ "mov rsp, rbp\npop rbp\nret\n\n"
                    )
                    $ filter (isJust . translatedText . snd) (zip [0 ..] translatedDefinitions)
        return $ dataSectionText ++ start ++ body ++ preludeAssembly

translateDefinition :: Name -> Maybe Int -> Maybe ReferentialType -> ParserWithDoubleState TranslationState Program ReturnType
translateDefinition name maybeIndex maybeReferentialType = do
    eitherDefinitions <- lift $ getDefinitionsFromName name
    let definition = either id (!! fromMaybe 0 maybeIndex) eitherDefinitions
    let referentialType = fromMaybe (definitionType definition) maybeReferentialType
    [(mainTypeWithValue, mainTypeReferences)] <- lift $ assertReferentialType referentialType $ definitionValue definition
    function <- translateInContext
        ( do
            modify $ \state ->
                state
                    { stateReferences = mainTypeReferences
                    , stateDefinitions = stateDefinitions state ++ [TranslatedDefinition (Just name) maybeIndex (Just referentialType) Nothing]
                    }
            translateValue mainTypeWithValue >>= callReturn "[rbp+16]"
        )
    return $ ReturnFunctionName function

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

loadDefinition :: Type -> Name -> Maybe Int -> ParserWithDoubleState TranslationState Program ReturnType
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
        Just (i, _) -> return $ ReturnFunctionName $ createFunctionName i
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
    write "push 16\ncall alloc\nadd rsp, 8\npush rax\n"
    translateValue x >>= returnInRax
    write "mov rdi, [rsp]\nmov [rdi], rax\n"
    translateValue y >>= returnInRax
    write "mov rdi, [rsp]\nmov [rdi+8], rax\npop rax\n"
    return ReturnValueInRax
translateValue (TypeWithSumLiteral _ boolChoice value) = do
    write "push 16\ncall alloc\nadd rsp, 8\npush rax\n"
    write $ "mov qword [rax], " ++ (if boolChoice then "1" else "0") ++ "\n"
    translateValue value >>= returnInRax
    write "mov rdi, [rsp]\nmov [rdi+8], rax\npop rax\n"
    return ReturnValueInRax
translateValue (TypeWithDefinedValue t name) = loadDefinition t name Nothing
translateValue (TypeWithDefinedValueFromInstance t name index) = loadDefinition t name (Just index)
translateValue (TypeWithUnaryArrowOperator Arr _ value) = translateValue value
translateValue (TypeWithUnaryArrowOperator ArrowConstant _ value) = translateAnonymous $ translateValue value >>= returnInRax
translateValue (TypeWithBinaryArrowOperator ArrowComposition _ x y) =
    translateAnonymous
        ( do
            translateValue x >>= callReturn "[rbp+16]"
            -- write "call rax\npush rax\n"
            translateValue y >>= callReturn "rax"
            -- write "mov rdi, rax\npop rax\ncall rdi\n"
        )
translateValue (TypeWithUnaryArrowOperator ArrowFirst _ value) =
    translateAnonymous
        ( do
            -- write "push rax\n"
            write "mov rax, [rbp+16]\n"
            translateValue value >>= callReturn "[rax]"
            -- write "mov rdi, rax\nmov rax, [rsp]\nmov rax, [rax]\ncall rdi\npop rdx\nmov [rdx], rax\nmov rax, rdx\n"
            write "mov rdx, [rbp+16]\nmov [rdx], rax\nmov rax, rdx\n"
        )
translateValue (TypeWithUnaryArrowOperator ArrowSecond _ value) =
    translateAnonymous
        ( do
            -- write "push rax\n"
            write "mov rax, [rbp+16]\n"
            translateValue value >>= callReturn "[rax+8]"
            -- write "mov rdi, rax\nmov rax, [rsp]\nmov rax, [rax+8]\ncall rdi\npop rdx\nmov [rdx+8], rax\nmov rax, rdx\n"
            write "mov rdx, [rbp+16]\nmov [rdx+8], rax\nmov rax, rdx\n"
        )
translateValue (TypeWithBinaryArrowOperator TripleAsterisks _ x y) =
    translateAnonymous
        ( do
            -- write "push rax\n"
            write "mov rax, [rbp+16]\n"
            translateValue x >>= callReturn "[rax]"
            -- write "mov rdi, rax\nmov rax, [rsp]\nmov rax, [rax]\ncall rdi\nmov rdx, [rsp]\nmov [rdx], rax\n"
            write "mov rdx, [rbp+16]\nmov [rdx], rax\n"
            translateValue y >>= callReturn "[rdx+8]"
            -- write "mov rdi, rax\nmov rax, [rdx+8]\ncall rdi\npop rdx\nmov [rdx+8], rax\nmov rax, rdx\n"
            write "mov rdx, [rbp+16]\nmov [rdx+8], rax\nmov rax, rdx\n"
        )
translateValue (TypeWithBinaryArrowOperator TripleAnd t x y) =
    translateAnonymous
        ( do
            write "push qword [rbp+16]\ncall double\n"
            translateValue (TypeWithBinaryArrowOperator TripleAsterisks t x y) >>= callReturn "rax"
        )
translateValue (TypeWithUnaryArrowOperator ArrowLeft _ value) =
    translateAnonymous
        ( do
            lable <- (".done_left" ++) . show <$> getNewLableIndex
            -- write $ "mov rcx, [rax]\ntest rcx, rcx\njnz " ++ lable ++ "\npush rax\n"
            write $ "mov rax, [rbp+16]\nmov rcx, [rax]\ntest rcx, rcx\njnz " ++ lable ++ "\n"
            translateValue value >>= callReturn "[rax+8]"
            -- write $ "mov rdi, rax\nmov rax, [rsp]\nmov rax, [rax+8]\ncall rdi\npop rdx\nmov [rdx+8], rax\nmov rax, rdx\n" ++ lable ++ ":\n"
            write $ "mov rdi, [rbp+16]\nmov [rdi+8], rax\nmov rax, rdi\n" ++ lable ++ ":\n"
        )
translateValue (TypeWithUnaryArrowOperator ArrowRight _ value) =
    translateAnonymous
        ( do
            lable <- (".done_right" ++) . show <$> getNewLableIndex
            -- write $ "mov rcx, [rax]\ntest rcx, rcx\njz " ++ lable ++ "\npush rax\n"
            write $ "mov rax, [rbp+16]\nmov rcx, [rax]\ntest rcx, rcx\njz " ++ lable ++ "\n"
            translateValue value >>= callReturn "[rax+8]"
            -- write $ "mov rdi, rax\nmov rax, [rsp]\nmov rax, [rax+8]\ncall rdi\npop rdx\nmov [rdx+8], rax\nmov rax, rdx\n" ++ lable ++ ":\n"
            write $ "mov rdi, [rbp+16]\nmov [rdi+8], rax\nmov rax, rdi\n" ++ lable ++ ":\n"
        )
translateValue (TypeWithBinaryArrowOperator TriplePlus _ x y) =
    translateAnonymous
        ( do
            lableIndex <- getNewLableIndex
            let caseLable = ".case_plus" ++ show lableIndex
            let doneLable = ".done_plus" ++ show lableIndex
            -- write $ "push rax\nmov rcx, [rax]\nmov rax, [rax+8]\ntest rcx, rcx\njnz " ++ caseLable ++ "\n"
            write $ "mov rax, [rbp+16]\nmov rcx, [rax]\ntest rcx, rcx\njnz " ++ caseLable ++ "\n"
            translateValue x >>= callReturn "[rax+8]"
            -- write $ "call rdi\njmp " ++ doneLable ++ "\n" ++ caseLable ++ ":\n"
            write $ "jmp " ++ doneLable ++ "\n" ++ caseLable ++ ":\n"
            translateValue y >>= callReturn "[rax+8]"
            -- write $ "call rdi\n" ++ doneLable ++ ":\npop rdi\nmov [rdi+8], rax\nmov rax, rdi\n"
            write $ doneLable ++ ":\nmov rdi, [rbp+16]\nmov [rdi+8], rax\nmov rax, rdi\n"
        )
translateValue (TypeWithBinaryArrowOperator TripleBar _ x y) =
    translateAnonymous
        ( do
            lableIndex <- getNewLableIndex
            let caseLable = ".case_bar" ++ show lableIndex
            let doneLable = ".done_bar" ++ show lableIndex
            -- write $ "mov rcx, [rax]\nmov rax, [rax+8]\ntest rcx, rcx\njnz " ++ caseLable ++ "\n"
            write $ "mov rax, [rbp+16]\nmov rcx, [rax]\ntest rcx, rcx\njnz " ++ caseLable ++ "\n"
            translateValue x >>= callReturn "[rax+8]"
            -- write $ "call rdi\njmp " ++ doneLable ++ "\n" ++ caseLable ++ ":\n"
            write $ "jmp " ++ doneLable ++ "\n" ++ caseLable ++ ":\n"
            translateValue y >>= callReturn "[rax+8]"
            -- write $ "call rdi\n" ++ doneLable ++ ":\n"
            write $ doneLable ++ ":\n"
        )
translateValue (TypeWithUndefined _) = write "mov rax, 60\nmov rdi, -1\nsyscall\n" $> ReturnValueInRax -- TODO: throw error

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
returnInRax (ReturnFunctionName name) = do
    write "push 16\ncall alloc\nadd rsp, 8\n"
    write "mov qword [rax], 0\n"
    write $ "lea rcx, " ++ name ++ "\nmov [rax+8], rcx\n"
    -- "lea rax, " ++ name ++ "\n"
returnInRax ReturnValueInRax = return ()

callReturn :: String -> ReturnType -> ParserWithDoubleState TranslationState Program ()
callReturn argument (ReturnFunctionName name) = write $ "push qword " ++ argument ++ "\ncall " ++ name ++ "\n"
callReturn argument ReturnValueInRax = write $ "push rax\npush qword " ++ argument ++ "\ncall call\n"

compilerDefinitions :: [(String, (Maybe Name, Name))]
compilerDefinitions =
    [ ("put_char", (Nothing, "putChar"))
    , ("read_char", (Nothing, "readChar"))
    , ("fst", (Nothing, "fst"))
    , ("snd", (Nothing, "snd"))
    , ("double", (Nothing, "double"))
    , ("id", (Just "Id", "id"))
    , ("app", (Just "Id", "app"))
    , ("add", (Nothing, "add"))
    , ("sub", (Nothing, "sub"))
    , ("mul", (Nothing, "mul"))
    , ("div", (Nothing, "div"))
    , ("mod", (Nothing, "mod"))
    , ("not", (Nothing, "not"))
    , ("and", (Nothing, "and"))
    , ("or", (Nothing, "or"))
    , ("l", (Nothing, "l"))
    , ("r", (Nothing, "r"))
    , ("id", (Nothing, "chr"))
    , ("id", (Nothing, "ord"))
    , ("id", (Nothing, "choice"))
    , ("eq", (Nothing, "=="))
    , ("less_int", (Nothing, "<"))
    , ("greater_int", (Nothing, ">"))
    , ("less_equ_int", (Nothing, "<="))
    , ("greater_equ_int",(Nothing, ">="))
    , ("first", (Nothing, "first"))
    , ("second", (Nothing, "second"))
    , ("composition", (Nothing, ">>>"))
    , ("triple_asterisk", (Nothing, "***"))
    , ("left", (Nothing, "left"))
    , ("right", (Nothing, "right"))
    , ("triple_and", (Nothing, "&&&"))
    , ("triple_plus", (Nothing, "+++"))
    , ("triple_bar", (Nothing, "|||"))
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

getIndexFromNameAndClass :: Name -> Name -> ParserWithState Program Int
getIndexFromNameAndClass className instanceName = gets $ fromMaybe 0 . elemIndex className . map instanceClassName . filter (elem instanceName . map fst . instanceMembers) . instances

-- gets $ find (\(Instance _ className' members) -> className == className' && instanceName elem $ map fst members) . instances

preludeAssembly :: String
preludeAssembly = $(embedStringFile "app/Prelude/Prelude.asm")
