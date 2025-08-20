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
import Control.Monad
import Text.Megaparsec

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
                ( ( \(maybeInstancType, name) -> case maybeInstancType of
                        (Just instanceType) -> TranslatedDefinition (Just name) . Just . head <$> getIndeciesFromNameAndInstancType name instanceType <*> return Nothing <*> return Nothing
                        Nothing -> return $ TranslatedDefinition (Just name) Nothing Nothing Nothing
                  )
                    . snd
                )
                compilerDefinitions
        (TranslationState _ _ translatedDefinitions dataSection _) <-
            execStateT
                ( do
                    translateDefinition "main" Nothing (ReferentialType (AliasReference "IO" [AliasReference "()" [], AliasReference "()" []]) [])
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

translateDefinition :: Name -> Maybe Int -> ReferentialType -> ParserWithDoubleState TranslationState Program ReturnType
translateDefinition name maybeIndex contextType = do
    eitherDefinitions <- lift $ getDefinitionsFromName name
    let definition = either id (!! fromMaybe 0 maybeIndex) eitherDefinitions
    maybeThisType <- maybe (return Nothing) (\index -> lift $ gets $ Just . instanceType . (!! index) . filter ((elem name) . map fst . instanceMembers) . instances) maybeIndex
    referentialType <- lift $ generalizeContext contextType (definitionType definition) maybeThisType
    -- let referentialType = fromMaybe (definitionType definition) maybeReferentialType
    (mainTypeWithValue, mainTypeReferences):_ <- lift $ assertReferentialType referentialType $ definitionValue definition
    -- x <- lift $ assertReferentialType referentialType $ definitionValue definition
    --case x of
    -- [(mainTypeWithValue, mainTypeReferences)] -> do
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
    --    _ -> fail $ show (displayReferentialType <$> maybeReferentialType)

generalizeContext :: ReferentialType -> ReferentialType -> Maybe ReferentialType -> ParserWithState Program ReferentialType
generalizeContext (ReferentialType frame contextReferences) fromDefinition maybeThisType = uncurry ReferentialType <$> runStateT f contextReferences -- ReferentialType (f context def) references
  where
    f = do
      let freeTypeVariableIndecies = map ((+ length contextReferences) . fst) $ filter (isFreeVariable . snd) $ zip [0..] $ otherTypes fromDefinition
      inferiorType <- addReferentialType $ toAnyTypeReferences fromDefinition maybeThisType
      typeGreaterThan frame inferiorType
      modify $ map (\(index, x) -> if index `elem` freeTypeVariableIndecies then AnyType index [] else x) . zip [0..]
      return frame 
    isFreeVariable (ForAllInstances _ []) = True
    isFreeVariable _ = False

    -- f (TypeReference index) x = f (references !! index) x
    -- f x (TypeReference index) = f x (references !! index)
    -- f (Product contextX contextY) (Product definitionX definitionY) = Product (f contextX definitionX) (f contextY definitionY)
    -- f (Sum contextX contextY) (Sum definitionX definitionY) = Sum (f contextX definitionX) (f contextY definitionY)
    -- f _ (ForAllInstances index []) = ForAllInstances index []
    -- f x _ = x

    -- | ForAllInstances Int [Name]
    -- | AnyType Int [Name]
    -- | AliasReference Name [Type]
    -- | AliasExtention Int [Type]

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
                    (translatedName translatedDefinition == Just name)
                        && (translatedIndex translatedDefinition == maybeIndex)
                        && (maybe True (== referentialType) (translatedType translatedDefinition))
                )
                . zip [0 ..]
                . stateDefinitions
    case maybeTranslatedDefinition of
        Just (i, _) -> return $ ReturnFunctionName $ createFunctionName i
        Nothing -> translateDefinition name maybeIndex referentialType

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
    write ("mov rax, [" ++ name ++ "]\n")
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
            write "mov rdx, rax\npush 16\ncall alloc\nmov [rax], rdx\nmov rdi, [rbp+16]\nmov rdi, [rdi+8]\nmov [rax+8], rdi\n"
        )
translateValue (TypeWithUnaryArrowOperator ArrowSecond _ value) =
    translateAnonymous
        ( do
            -- write "push rax\n"
            write "mov rax, [rbp+16]\n"
            translateValue value >>= callReturn "[rax+8]"
            -- write "mov rdi, rax\nmov rax, [rsp]\nmov rax, [rax+8]\ncall rdi\npop rdx\nmov [rdx+8], rax\nmov rax, rdx\n"
            -- write "mov rdx, [rbp+16]\nmov [rdx+8], rax\nmov rax, rdx\n"
            write "mov rdx, rax\npush 16\ncall alloc\nmov [rax+8], rdx\nmov rdi, [rbp+16]\nmov rdi, [rdi]\nmov [rax], rdi\n"
        )
translateValue (TypeWithBinaryArrowOperator TripleAsterisks _ x y) =
    translateAnonymous
        ( do
            -- write "push rax\n"
            write "mov rax, [rbp+16]\n"
            translateValue x >>= callReturn "[rax]"
            -- write "mov rdi, rax\nmov rax, [rsp]\nmov rax, [rax]\ncall rdi\nmov rdx, [rsp]\nmov [rdx], rax\n"
            -- write "mov rdx, [rbp+16]\nmov [rdx], rax\n"
            write "push rax\nmov rax, [rbp+16]\n"
            translateValue y >>= callReturn "[rax+8]"
            -- write "mov rdi, rax\nmov rax, [rdx+8]\ncall rdi\npop rdx\nmov [rdx+8], rax\nmov rax, rdx\n"
            -- write "mov rdx, [rbp+16]\nmov [rdx+8], rax\nmov rax, rdx\n"
            write "mov rdx, rax\npush 16\ncall alloc\nadd rsp, 8\npop rdi\nmov [rax], rdi\nmov [rax+8], rdx\n"
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
            -- write $ "mov rdi, [rbp+16]\nmov [rdi+8], rax\nmov rax, rdi\n" ++ lable ++ ":\n"
            write $ "mov rdx, rax\npush 16\ncall alloc\nmov qword [rax], 0\nmov [rax+8], rdx\n" ++ lable ++ ":\n"
        )
translateValue (TypeWithUnaryArrowOperator ArrowRight _ value) =
    translateAnonymous
        ( do
            lable <- (".done_right" ++) . show <$> getNewLableIndex
            -- write $ "mov rcx, [rax]\ntest rcx, rcx\njz " ++ lable ++ "\npush rax\n"
            write $ "mov rax, [rbp+16]\nmov rcx, [rax]\ntest rcx, rcx\njz " ++ lable ++ "\n"
            translateValue value >>= callReturn "[rax+8]"
            -- write $ "mov rdi, rax\nmov rax, [rsp]\nmov rax, [rax+8]\ncall rdi\npop rdx\nmov [rdx+8], rax\nmov rax, rdx\n" ++ lable ++ ":\n"
            -- write $ "mov rdi, [rbp+16]\nmov [rdi+8], rax\nmov rax, rdi\n" ++ lable ++ ":\n"
            write $ "mov rdx, rax\npush 16\ncall alloc\nmov qword [rax], 1\nmov [rax+8], rdx\n" ++ lable ++ ":\n"
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
            -- write $ "jmp " ++ doneLable ++ "\n" ++ caseLable ++ ":\n"
            write $ "mov rdx, rax\npush 16\ncall alloc\nmov qword [rax], 0\nmov [rax+8], rdx\njmp " ++ doneLable ++ "\n" ++ caseLable ++ ":\n"
            translateValue y >>= callReturn "[rax+8]"
            -- write $ "call rdi\n" ++ doneLable ++ ":\npop rdi\nmov [rdi+8], rax\nmov rax, rdi\n"
            -- write $ doneLable ++ ":\nmov rdi, [rbp+16]\nmov [rdi+8], rax\nmov rax, rdi\n"
            write $ "mov rdx, rax\npush 16\ncall alloc\nmov qword [rax], 1\nmov [rax+8], rdx\n" ++ doneLable ++ ":\n"
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
translateValue (TypeWithUndefined _ sourcePos) = write ("mov rax, 60\nmov rdi, " ++ show (unPos $ sourceLine sourcePos) ++ "\nsyscall\n") $> ReturnValueInRax -- TODO: throw error

-- show (unPos $ sourceLine sourcePos) 

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
callReturn argument (ReturnFunctionName name) = write $ "push qword " ++ argument ++ "\ncall " ++ name ++ "\nadd rsp, 8\n"
callReturn argument ReturnValueInRax = write $ "push rax\npush qword " ++ argument ++ "\ncall call\nadd rsp, 16\n"

compilerDefinitions :: [(String, (Maybe ReferentialType, Name))]
compilerDefinitions =
    [ ("put_char", (Nothing, "putChar"))
    , ("read_char", (Nothing, "readChar"))
    , ("fst", (Nothing, "fst"))
    , ("snd", (Nothing, "snd"))
    , ("double", (Nothing, "double"))
    , ("id", (Just $ idArrowType 0, "id"))
    , ("app", (Just $ idArrowType 0, "app"))
    , ("add_int", (Just $ ReferentialType (AliasReference "Int" []) [], "+"))
    , ("sub_int", (Just $ ReferentialType (AliasReference "Int" []) [], "-"))
    , ("mul_int", (Just $ ReferentialType (AliasReference "Int" []) [], "*"))
    , ("div_int", (Just $ ReferentialType (AliasReference "Int" []) [], "/"))
    , ("abs_int", (Just $ ReferentialType (AliasReference "Int" []) [], "abs"))
    , ("neg_int", (Just $ ReferentialType (AliasReference "Int" []) [], "neg"))
    , ("add_float", (Just $ ReferentialType (AliasReference "Float" []) [], "+"))
    , ("sub_float", (Just $ ReferentialType (AliasReference "Float" []) [], "-"))
    , ("mul_float", (Just $ ReferentialType (AliasReference "Float" []) [], "*"))
    , ("div_folat", (Just $ ReferentialType (AliasReference "Float" []) [], "/"))
    , ("abs_folat", (Just $ ReferentialType (AliasReference "Float" []) [], "abs"))
    , ("neg_folat", (Just $ ReferentialType (AliasReference "Float" []) [], "neg"))
    , ("mod", (Nothing, "%"))
    , ("cos", (Nothing, "cos"))
    , ("tan", (Nothing, "tan"))
    , ("not", (Nothing, "not"))
    , ("and", (Nothing, "&&"))
    , ("or", (Nothing, "||"))
    , ("and", (Nothing, "&"))
    , ("or", (Nothing, "|"))
    , ("xor", (Nothing, "^"))
    , ("l", (Nothing, "l"))
    , ("r", (Nothing, "r"))
    , ("id", (Nothing, "chr"))
    , ("id", (Nothing, "ord"))
    , ("id", (Nothing, "choice"))
    , ("eq", (Just $ ReferentialType (AliasReference "Bool" []) [], "=="))
    , ("eq", (Just $ ReferentialType (AliasReference "Char" []) [], "=="))
    , ("eq", (Just $ ReferentialType (AliasReference "Int" []) [], "=="))
    , ("less_int", (Just $ ReferentialType (AliasReference "Int" []) [], "<"))
    , ("greater_int", (Just $ ReferentialType (AliasReference "Int" []) [], ">"))
    , ("less_equ_int", (Just $ ReferentialType (AliasReference "Int" []) [], "<="))
    , ("greater_equ_int",(Just $ ReferentialType (AliasReference "Int" []) [], ">="))
    , ("eq_float", (Just $ ReferentialType (AliasReference "Float" []) [], "=="))
    , ("less_float", (Just $ ReferentialType (AliasReference "Float" []) [], "<"))
    , ("greater_float", (Just $ ReferentialType (AliasReference "Float" []) [], ">"))
    , ("less_equ_float", (Just $ ReferentialType (AliasReference "Float" []) [], "<="))
    , ("greater_equ_float",(Just $ ReferentialType (AliasReference "Float" []) [], ">="))
    , ("round_float_to_int",(Nothing, "roundToInt"))
    -- , ("is_left",(Nothing, "isLeft"))
    -- , ("is_right",(Nothing, "isRight"))
    , ("first", (Just $ idArrowType 0, "first"))
    , ("second", (Just $ idArrowType 0, "second"))
    , ("composition", (Just $ idArrowType 0, ">>>"))
    , ("triple_asterisk", (Just $ idArrowType 0, "***"))
    , ("left", (Just $ idArrowType 0, "left"))
    , ("right", (Just $ idArrowType 0, "right"))
    , ("triple_and", (Just $ idArrowType 0, "&&&"))
    , ("triple_plus", (Just $ idArrowType 0, "+++"))
    , ("triple_bar", (Just $ idArrowType 0, "|||"))
    , ("swap", (Nothing, "swap"))
    , ("swap_choice", (Nothing, "swapChoice"))
    , ("reorder_to_front", (Nothing, "reorderToFront"))
    , ("reorder_to_back", (Nothing, "reorderToBack"))
    , ("expand_choice_left", (Nothing, "expandChoiceLeft"))
    , ("expand_choice_right", (Nothing, "expandChoiceRight"))
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

-- getIndexFromNameAndClass :: Name -> Name -> ParserWithState Program Int
-- getIndexFromNameAndClass instanceName memberName = gets $ fromMaybe 0 . elemIndex instanceName . map instanceClassName . filter (elem memberName . map fst . instanceMembers) . instances
-- instancesWithName <- lift $ gets $ filter (elem name . map fst . instanceMembers) . instancesWithName
-- let possibleInstances = filter ((== instancedType) . instanceType . snd) $ zip [0..] instancesWithName

-- gets $ find (\(Instance _ className' members) -> className == className' && instanceName elem $ map fst members) . instances

preludeAssembly :: String
preludeAssembly = $(embedStringFile "app/Prelude/Prelude.asm")
