{-# LANGUAGE TemplateHaskell #-}

module Translator where

import Ast
import Checker.Types
import Control.Monad
import Control.Monad.State hiding (state)
import Data.Char (ord)
import Data.FileEmbed (embedStringFile)
import Data.Functor
import Data.List
import Data.Maybe
import Helpers
import Parser.Primitives
import Text.Megaparsec

data TranslationState = TranslationState {stateReferences :: [Type], stateText :: String, stateDefinitions :: [TranslatedDefinition], stateTextSection :: [String], stateDataSection :: [DataSection], stateLabelIndex :: Int, stateFunctionIndex :: Int}

data DataSection = FloatData Double | StringData String | ProductData String String | SumData Bool String
  deriving (Show, Eq)

data TranslatedDefinition = TranslatedDefinition {translatedName :: Maybe Name, translatedIndex :: Maybe Int, translatedType :: Maybe ReferentialType, translatedAsmName :: String}
  deriving (Show, Eq, Ord)

translate :: ParserWithState Program String
translate = do
  compilerDefinitionsAsTranslated <-
    mapM
      ( \(CompilerDefinition asmName maybeInstancType name) -> case maybeInstancType of
          (Just t) -> TranslatedDefinition (Just name) . Just . head <$> getIndeciesFromNameAndInstanceType name t <*> return Nothing <*> return asmName
          Nothing -> return $ TranslatedDefinition (Just name) Nothing Nothing asmName
      )
      compilerDefinitions
  (mainAsmName, TranslationState _ _ _ textSection dataSection _ _) <-
    runStateT
      (loadDefinition (AliasReference "IO" [AliasReference "()" [], AliasReference "()" []]) "main" Nothing)
      (TranslationState [] "" compilerDefinitionsAsTranslated [] [] 0 0)
  let dataSectionText = "section .data\n" ++ unlines (zipWith dataSectionToText [0 ..] dataSection) ++ "\n"
  let start = "section .text\nglobal _start\n_start:\npush rbp\nmov rbp, rsp\ncall " ++ mainAsmName ++ "\nmov rsp, rbp\npop rbp\nmov rax, 60\nxor rdi, rdi\nsyscall\n\n"
  let body = unlines textSection
  return $ dataSectionText ++ start ++ body ++ preludeAssembly

-- error $ unlines $ map (\x -> show (translatedName x) ++ " " ++ displayReferentialType (fromJust (translatedType x))) $ filter (isJust . translatedType) defs

translateDefinition :: Name -> Maybe Int -> ReferentialType -> ParserWithDoubleState TranslationState Program String
translateDefinition name maybeIndex contextType = do
  eitherDefinitions <- lift $ getDefinitionsFromName name
  let definition = either id (!! fromMaybe 0 maybeIndex) eitherDefinitions
  maybeThisType <- maybe (return Nothing) (\index -> lift $ gets $ Just . instanceType . (!! index) . filter (elem name . map fst . instanceMembers) . instances) maybeIndex
  referentialType <- lift $ generalizeContext contextType (definitionType definition) maybeThisType
  (maybeMainTypeWithValue, mainTypeReferences) : _ <- lift $ assertReferentialType referentialType $ definitionValue definition
  -- when (name == "show") $ error $ show (definitionValue definition)
  translationIndex <- gets $ length . stateDefinitions
  case maybeMainTypeWithValue of
    Nothing -> undefined
    -- modify $ \state ->
    --     state
    --         { stateReferences = mainTypeReferences
    --           , stateDefinitions = stateDefinitions state ++ [TranslatedDefinition (Just name) maybeIndex (Just referentialType) "id"]
    --         }
    -- return "id"
    -- error $ show contextType
    (Just mainTypeWithValue) -> do
      namePreview <- fromMaybe "" <$> translateValuePreview mainTypeWithValue
      backupReferences <- gets stateReferences
      modify $ \state ->
        state
          { stateReferences = mainTypeReferences,
            stateDefinitions = stateDefinitions state ++ [TranslatedDefinition (Just name) maybeIndex (Just referentialType) namePreview]
          }
      asmName <- translateValue mainTypeWithValue
      modify $ \state ->
        state
          { stateReferences = backupReferences,
            stateDefinitions = replace translationIndex (TranslatedDefinition (Just name) maybeIndex (Just referentialType) asmName) (stateDefinitions state)
          }
      return asmName

generalizeContext :: ReferentialType -> ReferentialType -> Maybe ReferentialType -> ParserWithState Program ReferentialType
generalizeContext (ReferentialType frame contextReferences) fromDefinition maybeThisType = uncurry ReferentialType <$> runStateT f contextReferences
  where
    f = do
      let freeTypeVariableIndecies = map ((+ length contextReferences) . fst) $ filter (isFreeVariable . snd) $ zip [0 ..] $ otherTypes fromDefinition
      inferiorType <- addReferentialType $ toAnyTypeReferences fromDefinition maybeThisType
      typeGreaterThan frame inferiorType
      modify $ zipWith (\index x -> if index `elem` freeTypeVariableIndecies then AnyType index [] else x) [0 ..]
      return frame
    isFreeVariable (ForAllInstances _ []) = True
    isFreeVariable _ = False

translateAnonymousFunction :: ParserWithDoubleState TranslationState Program () -> ParserWithDoubleState TranslationState Program String
translateAnonymousFunction action = do
  name <- createFunctionName
  backupReferences <- gets stateReferences
  backupText <- gets stateText
  modify $ \state ->
    state
      { stateDefinitions = stateDefinitions state ++ [TranslatedDefinition Nothing Nothing Nothing name],
        stateText = ""
      }
  action
  endFunction name
  modify $ \state ->
    state
      { stateReferences = backupReferences,
        stateText = backupText
      }
  return name

loadDefinition :: Type -> Name -> Maybe Int -> ParserWithDoubleState TranslationState Program String
loadDefinition t name maybeIndex = do
  referentialType <- gets $ ReferentialType t . stateReferences
  existingDefinitions <- gets $ filter (\translatedDefinition -> (translatedName translatedDefinition == Just name) && (translatedIndex translatedDefinition == maybeIndex)) . stateDefinitions
  -- maybeTranslatedDefinition <-
  --     gets $
  --         find
  --             ( \translatedDefinition ->
  --                 (translatedName translatedDefinition == Just name)
  --                     && (translatedIndex translatedDefinition == maybeIndex)
  --                     && (maybe True (== referentialType) (translatedType translatedDefinition))
  --             )
  --             . stateDefinitions
  references <- gets stateReferences
  maybeTranslatedDefinition <-
    join . find isJust
      <$> mapM
        ( \translatedDefinition -> case translatedType translatedDefinition of
            (Just translatedReferentialType) ->
              lift $
                evalStateT
                  ( ( do
                        t' <- addReferentialType translatedReferentialType
                        typeGreaterThan t t'
                        return $ Just translatedDefinition
                    )
                      <|> return Nothing
                  )
                  references
            Nothing -> return $ Just translatedDefinition
        )
        existingDefinitions
  -- showTranslations <- gets $ filter ((== Just "show") . translatedName) . stateDefinitions
  -- when (name == "show" && (not $ null showTranslations)) $ do
  --     let x = translatedType $ head showTranslations
  --     error $ show x
  case maybeTranslatedDefinition of
    Just (TranslatedDefinition _ _ _ asmName) -> return asmName
    Nothing -> translateDefinition name maybeIndex referentialType

loadDefinitionPreview :: Type -> Name -> Maybe Int -> ParserWithDoubleState TranslationState Program (Maybe String)
loadDefinitionPreview t name maybeIndex = do
  referentialType <- gets $ ReferentialType t . stateReferences
  gets $
    fmap translatedAsmName
      . find
        ( \translatedDefinition ->
            (translatedName translatedDefinition == Just name)
              && (translatedIndex translatedDefinition == maybeIndex)
              && maybe True (== referentialType) (translatedType translatedDefinition)
        )
      . stateDefinitions

translateValue :: TypeWithValue -> ParserWithDoubleState TranslationState Program String
translateValue (TypeWithIntLiteral _ value) = return $ show value
translateValue (TypeWithCharLiteral _ value) = return $ show (ord value)
translateValue (TypeWithBoolLiteral _ value) = return $ if value then "1" else "0"
translateValue (TypeWithEmptyTupleLiteral _) = return "0"
translateValue (TypeWithFloatLiteral _ value) = do
  addDataSection $ FloatData value
translateValue (TypeWithProductLiteral _ x y) = do
  xName <- translateValue x
  yName <- translateValue y
  addDataSection $ ProductData xName yName
translateValue (TypeWithSumLiteral _ boolChoice value) = do
  innerName <- translateValue value
  addDataSection $ SumData boolChoice innerName
translateValue (TypeWithDefinedValue t name) = loadDefinition t name Nothing
translateValue (TypeWithDefinedValueFromInstance t name index) = loadDefinition t name (Just index)
translateValue (TypeWithUnaryArrowOperator Arr _ value) = translateValue value
translateValue (TypeWithUnaryArrowOperator ArrowConstant _ value) = translateAnonymousFunction $ translateValue value >>= returnValueInRax
-- \| t == (AliasReference "Float" []) = translateAnonymousFunction $ translateValue value >>= returnValueInRax >> write "mov rax, [rax]\n"
-- \| otherwise = translateAnonymousFunction $ translateValue value >>= returnValueInRax
translateValue (TypeWithBinaryArrowOperator ArrowComposition _ x y) =
  translateAnonymousFunction
    ( do
        translateValue x >>= callReturn "[rbp+16]"
        translateValue y >>= callReturn "rax"
    )
translateValue (TypeWithUnaryArrowOperator ArrowFirst _ value) =
  translateAnonymousFunction
    ( do
        write "mov rax, [rbp+16]\n"
        translateValue value >>= callReturn "[rax]"
        write "mov rdx, rax\npush 16\ncall alloc\nmov [rax], rdx\nmov rdi, [rbp+16]\nmov rdi, [rdi+8]\nmov [rax+8], rdi\n"
    )
translateValue (TypeWithUnaryArrowOperator ArrowSecond _ value) =
  translateAnonymousFunction
    ( do
        write "mov rax, [rbp+16]\n"
        translateValue value >>= callReturn "[rax+8]"
        write "mov rdx, rax\npush 16\ncall alloc\nmov [rax+8], rdx\nmov rdi, [rbp+16]\nmov rdi, [rdi]\nmov [rax], rdi\n"
    )
translateValue (TypeWithBinaryArrowOperator TripleAsterisks _ x y) =
  translateAnonymousFunction
    ( do
        write "mov rax, [rbp+16]\n"
        translateValue x >>= callReturn "[rax]"
        write "push rax\nmov rax, [rbp+16]\n"
        translateValue y >>= callReturn "[rax+8]"
        write "mov rdx, rax\npush 16\ncall alloc\nadd rsp, 8\npop rdi\nmov [rax], rdi\nmov [rax+8], rdx\n"
    )
translateValue (TypeWithBinaryArrowOperator TripleAnd t x y) =
  translateAnonymousFunction
    ( do
        write "push qword [rbp+16]\ncall double\n"
        translateValue (TypeWithBinaryArrowOperator TripleAsterisks t x y) >>= callReturn "rax"
    )
translateValue (TypeWithUnaryArrowOperator ArrowLeft _ value) =
  translateAnonymousFunction
    ( do
        lable <- (".done_left" ++) . show <$> getNewLableIndex
        write $ "mov rax, [rbp+16]\nmov rcx, [rax]\ntest rcx, rcx\njnz " ++ lable ++ "\n"
        translateValue value >>= callReturn "[rax+8]"
        write $ "mov rdx, rax\npush 16\ncall alloc\nmov qword [rax], 0\nmov [rax+8], rdx\n" ++ lable ++ ":\n"
    )
translateValue (TypeWithUnaryArrowOperator ArrowRight _ value) =
  translateAnonymousFunction
    ( do
        lable <- (".done_right" ++) . show <$> getNewLableIndex
        write $ "mov rax, [rbp+16]\nmov rcx, [rax]\ntest rcx, rcx\njz " ++ lable ++ "\n"
        translateValue value >>= callReturn "[rax+8]"
        write $ "mov rdx, rax\npush 16\ncall alloc\nmov qword [rax], 1\nmov [rax+8], rdx\n" ++ lable ++ ":\n"
    )
translateValue (TypeWithBinaryArrowOperator TriplePlus _ x y) =
  translateAnonymousFunction
    ( do
        lableIndex <- getNewLableIndex
        let caseLable = ".case_plus" ++ show lableIndex
        let doneLable = ".done_plus" ++ show lableIndex
        write $ "mov rax, [rbp+16]\nmov rcx, [rax]\ntest rcx, rcx\njnz " ++ caseLable ++ "\n"
        translateValue x >>= callReturn "[rax+8]"
        write $ "mov rdx, rax\npush 16\ncall alloc\nmov qword [rax], 0\nmov [rax+8], rdx\njmp " ++ doneLable ++ "\n" ++ caseLable ++ ":\n"
        translateValue y >>= callReturn "[rax+8]"
        write $ "mov rdx, rax\npush 16\ncall alloc\nmov qword [rax], 1\nmov [rax+8], rdx\n" ++ doneLable ++ ":\n"
    )
translateValue (TypeWithBinaryArrowOperator TripleBar _ x y) =
  translateAnonymousFunction
    ( do
        lableIndex <- getNewLableIndex
        let caseLable = ".case_bar" ++ show lableIndex
        let doneLable = ".done_bar" ++ show lableIndex
        write $ "mov rax, [rbp+16]\nmov rcx, [rax]\ntest rcx, rcx\njnz " ++ caseLable ++ "\n"
        translateValue x >>= callReturn "[rax+8]"
        write $ "jmp " ++ doneLable ++ "\n" ++ caseLable ++ ":\n"
        translateValue y >>= callReturn "[rax+8]"
        write $ doneLable ++ ":\n"
    )
translateValue (TypeWithUndefined _ sourcePos) = write ("mov rax, 60\nmov rdi, " ++ show (unPos $ sourceLine sourcePos) ++ "\nsyscall\n") $> "0" -- TODO: throw error
translateValue (TypeWithUntranslateable _) = return "id"

translateValuePreview :: TypeWithValue -> ParserWithDoubleState TranslationState Program (Maybe String)
translateValuePreview (TypeWithProductLiteral _ x y) = do
  xName <- translateValuePreview x
  yName <- translateValuePreview y
  Just <$> addDataSectionPreview (ProductData <$> xName <*> yName)
translateValuePreview (TypeWithSumLiteral _ boolChoice value) = do
  innerName <- translateValuePreview value
  Just <$> addDataSectionPreview (SumData boolChoice <$> innerName)
translateValuePreview (TypeWithDefinedValue t name) = loadDefinitionPreview t name Nothing
translateValuePreview (TypeWithDefinedValueFromInstance t name index) = loadDefinitionPreview t name (Just index)
translateValuePreview (TypeWithBinaryArrowOperator {}) = Just <$> createFunctionNamePrewiev
translateValuePreview (TypeWithUnaryArrowOperator {}) = Just <$> createFunctionNamePrewiev
translateValuePreview _ = return Nothing

write :: String -> ParserWithDoubleState TranslationState Program ()
write newText = modify $ \state -> state {stateText = stateText state ++ newText}

addDataSection :: DataSection -> ParserWithDoubleState TranslationState Program String
addDataSection value = do
  maybeIndex <- gets $ fmap fst . find ((== value) . snd) . zip [0 ..] . stateDataSection
  createConstantName <$> case maybeIndex of
    (Just i) -> return i
    Nothing -> do
      index <- gets $ length . stateDataSection
      modify $ \state -> state {stateDataSection = stateDataSection state ++ [value]}
      return index

addDataSectionPreview :: Maybe DataSection -> ParserWithDoubleState TranslationState Program String
addDataSectionPreview value = do
  maybeIndex <- gets $ fmap fst . find ((== value) . Just . snd) . zip [0 ..] . stateDataSection
  createConstantName <$> case maybeIndex of
    (Just i) -> return i
    Nothing -> gets $ length . stateDataSection

endFunction :: String -> ParserWithDoubleState TranslationState Program ()
endFunction name = do
  modify $ \state -> state {stateText = "", stateTextSection = stateTextSection state ++ [name ++ ":\npush rbp\nmov rbp, rsp\n" ++ stateText state ++ "mov rsp, rbp\npop rbp\nret\n"]}

dataSectionToText :: Int -> DataSection -> String
dataSectionToText index (FloatData value) = createConstantName index ++ " dq " ++ show value
dataSectionToText index (StringData value) = createConstantName index ++ " db " ++ show value ++ ", 0"
dataSectionToText index (ProductData xName yName) = createConstantName index ++ " dq " ++ xName ++ ", " ++ yName
dataSectionToText index (SumData boolChoice innerName) = createConstantName index ++ " dq " ++ (if boolChoice then "1" else "0") ++ ", " ++ innerName

returnValueInRax :: String -> ParserWithDoubleState TranslationState Program ()
returnValueInRax name = write $ "mov rax, " ++ name ++ "\n"

callReturn :: String -> String -> ParserWithDoubleState TranslationState Program ()
callReturn argument name = write $ "push qword " ++ argument ++ "\ncall " ++ name ++ "\nadd rsp, 8\n"

data CompilerDefinition = CompilerDefinition {compilerDefinitionAsmName :: String, compilerDefinitionInstanceType :: Maybe ReferentialType, compilerDefinitionName :: Name}

compilerDefinitions :: [CompilerDefinition]
compilerDefinitions =
  [ CompilerDefinition "put_char" Nothing "putChar",
    CompilerDefinition "read_char" Nothing "readChar",
    CompilerDefinition "fst" Nothing "fst",
    CompilerDefinition "snd" Nothing "snd",
    CompilerDefinition "double" Nothing "double",
    CompilerDefinition "id" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "id",
    CompilerDefinition "id" (Just $ idArrowType 0) "id",
    CompilerDefinition "app" (Just $ idArrowType 0) "app",
    CompilerDefinition "add_int" (Just $ ReferentialType (AliasReference "Int" []) []) "+",
    CompilerDefinition "sub_int" (Just $ ReferentialType (AliasReference "Int" []) []) "-",
    CompilerDefinition "mul_int" (Just $ ReferentialType (AliasReference "Int" []) []) "*",
    CompilerDefinition "div_int" (Just $ ReferentialType (AliasReference "Int" []) []) "/",
    CompilerDefinition "abs_int" (Just $ ReferentialType (AliasReference "Int" []) []) "abs",
    CompilerDefinition "neg_int" (Just $ ReferentialType (AliasReference "Int" []) []) "neg",
    CompilerDefinition "add_float" (Just $ ReferentialType (AliasReference "Float" []) []) "+",
    CompilerDefinition "sub_float" (Just $ ReferentialType (AliasReference "Float" []) []) "-",
    CompilerDefinition "mul_float" (Just $ ReferentialType (AliasReference "Float" []) []) "*",
    CompilerDefinition "div_float" (Just $ ReferentialType (AliasReference "Float" []) []) "/",
    CompilerDefinition "abs_float" (Just $ ReferentialType (AliasReference "Float" []) []) "abs",
    CompilerDefinition "neg_float" (Just $ ReferentialType (AliasReference "Float" []) []) "neg",
    CompilerDefinition "mod" Nothing "%",
    CompilerDefinition "not" Nothing "not",
    CompilerDefinition "and" Nothing "&&",
    CompilerDefinition "or" Nothing "||",
    CompilerDefinition "and" Nothing "&",
    CompilerDefinition "or" Nothing "|",
    CompilerDefinition "xor" Nothing "^",
    CompilerDefinition "l" Nothing "l",
    CompilerDefinition "r" Nothing "r",
    CompilerDefinition "id" Nothing "chr",
    CompilerDefinition "id" Nothing "ord",
    CompilerDefinition "id" Nothing "choice",
    CompilerDefinition "eq" (Just $ ReferentialType (AliasReference "Bool" []) []) "==",
    CompilerDefinition "eq" (Just $ ReferentialType (AliasReference "Char" []) []) "==",
    CompilerDefinition "eq" (Just $ ReferentialType (AliasReference "Int" []) []) "==",
    CompilerDefinition "less_int" (Just $ ReferentialType (AliasReference "Int" []) []) "<",
    CompilerDefinition "greater_int" (Just $ ReferentialType (AliasReference "Int" []) []) ">",
    CompilerDefinition "less_eq_int" (Just $ ReferentialType (AliasReference "Int" []) []) "<=",
    CompilerDefinition "greater_eq_int" (Just $ ReferentialType (AliasReference "Int" []) []) ">=",
    CompilerDefinition "eq_float" (Just $ ReferentialType (AliasReference "Float" []) []) "==",
    CompilerDefinition "less_float" (Just $ ReferentialType (AliasReference "Float" []) []) "<",
    CompilerDefinition "greater_float" (Just $ ReferentialType (AliasReference "Float" []) []) ">",
    CompilerDefinition "less_eq_float" (Just $ ReferentialType (AliasReference "Float" []) []) "<=",
    CompilerDefinition "greater_eq_float" (Just $ ReferentialType (AliasReference "Float" []) []) ">=",
    CompilerDefinition "float_to_int" Nothing "float2Int",
    CompilerDefinition "first" (Just $ idArrowType 0) "first",
    CompilerDefinition "second" (Just $ idArrowType 0) "second",
    CompilerDefinition "composition" (Just $ idArrowType 0) ">>>",
    CompilerDefinition "triple_asterisk" (Just $ idArrowType 0) "***",
    CompilerDefinition "left" (Just $ idArrowType 0) "left",
    CompilerDefinition "right" (Just $ idArrowType 0) "right",
    CompilerDefinition "triple_and" (Just $ idArrowType 0) "&&&",
    CompilerDefinition "triple_plus" (Just $ idArrowType 0) "+++",
    CompilerDefinition "triple_bar" (Just $ idArrowType 0) "|||",
    CompilerDefinition "first" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "first",
    CompilerDefinition "second" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "second",
    CompilerDefinition "composition" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) ">>>",
    CompilerDefinition "triple_asterisk" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "***",
    CompilerDefinition "left" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "left",
    CompilerDefinition "right" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "right",
    CompilerDefinition "triple_and" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "&&&",
    CompilerDefinition "triple_plus" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "+++",
    CompilerDefinition "triple_bar" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "|||",
    CompilerDefinition "swap" Nothing "swap",
    CompilerDefinition "swap_choice" Nothing "swapChoice",
    CompilerDefinition "reorder_to_front" Nothing "reorderToFront",
    CompilerDefinition "reorder_to_back" Nothing "reorderToBack",
    CompilerDefinition "include_left" Nothing "includeLeft",
    CompilerDefinition "include_right" Nothing "includeRight"
  ]

createFunctionName :: ParserWithDoubleState TranslationState Program String
createFunctionName = do
  index <- gets stateFunctionIndex
  modify $ \state -> state {stateFunctionIndex = index + 1}
  return $ "fun" ++ show index

createFunctionNamePrewiev :: ParserWithDoubleState TranslationState Program String
createFunctionNamePrewiev = do
  index <- gets stateFunctionIndex
  return $ "fun" ++ show index

createConstantName :: Int -> String
createConstantName index = "const" ++ show index

getNewLableIndex :: ParserWithDoubleState TranslationState Program Int
getNewLableIndex = do
  index <- gets stateLabelIndex
  modify $ \state -> state {stateLabelIndex = index + 1}
  return index

preludeAssembly :: String
preludeAssembly = $(embedStringFile "app/Prelude/Prelude.asm")
