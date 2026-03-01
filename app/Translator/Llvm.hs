{-# LANGUAGE TemplateHaskell #-}

module Translator.Llvm where

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
import Data.Text.Lazy.Read (double)

data TranslationState = TranslationState {stateReferences :: [Type], stateText :: String, stateDefinitions :: [TranslatedDefinition], stateTextSection :: [String], stateDataSection :: [(Int, DataSection)], stateRegisterIndex :: Int, stateLabelIndex :: Int, stateFunctionIndex :: Int, stateDataIndex :: Int}

data DataSection = FloatData Double | IntData Int | ProductData String String | SumData Bool String
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
  (mainAsmName, TranslationState _ _ _ textSection dataSection _ _ _ _) <-
    runStateT
      (loadDefinition (AliasReference "IO" [AliasReference "()" [], AliasReference "()" []]) "main" Nothing)
      (TranslationState [] "" compilerDefinitionsAsTranslated [] [] 0 0 0 0)
  let dataSectionText = unlines (map (uncurry dataSectionToText) dataSection) ++ "\n"
  let start = "define i32 @main() {\ncall void " ++ mainAsmName ++ "()\nret i32 0\n}\n\n"
  let body = unlines textSection
  return $ preludeLlvm ++ dataSectionText ++ start ++ body
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

translateAnonymousFunction :: ParserWithDoubleState TranslationState Program (String, String) -> ParserWithDoubleState TranslationState Program String
translateAnonymousFunction action = do
  name <- createFunctionName
  backupReferences <- gets stateReferences
  backupText <- gets stateText
  modify $ \state ->
    state
      { stateDefinitions = stateDefinitions state ++ [TranslatedDefinition Nothing Nothing Nothing name],
        stateText = ""
      }
  action >>= endFunction name
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
-- translateValue (TypeWithIntLiteral _ value) = addDataSection $ return $ IntData value
  -- reg <- createRegisterName
  -- write (reg ++ " = add i64 0, " ++ show value ++ "\n")
  -- return reg
-- translateValue (TypeWithCharLiteral _ value) = addDataSection $ return $ IntData $ ord value
  -- reg <- createRegisterName
  -- write (reg ++ " = add i64 0, " ++ show (ord value) ++ "\n")
  -- return reg
-- translateValue (TypeWithBoolLiteral _ value) =  addDataSection $ return $ IntData $ if value then 1 else 0
  -- reg <- createRegisterName
  -- write (reg ++ " = add i64 0, " ++ (if value then "1" else "0") ++ "\n")
  -- return reg
-- translateValue (TypeWithEmptyTupleLiteral _) =  return "@zero"
  -- reg <- createRegisterName
  -- write (reg ++ " = add i64 0, 0\n")
  -- return reg
translateValue (TypeWithFloatLiteral _ value) = return $ show value -- addDataSection $ return $ FloatData value
translateValue (TypeWithProductLiteral _ x y) = do
  addDataSection (do
    xName <- translateValue x
    yName <- translateValue y
    return $ ProductData xName yName)
translateValue (TypeWithSumLiteral _ boolChoice value) = do
  addDataSection (do
    innerName <- translateValue value
    return $ SumData boolChoice innerName)
translateValue (TypeWithDefinedValue t name) = loadDefinition t name Nothing
translateValue (TypeWithDefinedValueFromInstance t name index) = loadDefinition t name (Just index)
translateValue (TypeWithUnaryArrowOperator Arr _ value) = translateValue value
translateValue (TypeWithUnaryArrowOperator ArrowConstant _ value) = translateAnonymousFunction 
  ( do
    retType <- translateValue value >>= returnValue
    return ("i64", retType)
  )
-- \| t == (AliasReference "Float" []) = translateAnonymousFunction $ translateValue value >>= returnValueInRax >> write "mov rax, [rax]\n"
-- \| otherwise = translateAnonymousFunction $ translateValue value >>= returnValueInRax
translateValue (TypeWithBinaryArrowOperator ArrowComposition _ x y) =
  translateAnonymousFunction
    ( do
        xName <- translateValue x
        write $ "%mid = call i64 " ++ xName ++ "(i64 %arg)\n";
        yName <- translateValue y
        write $ "%res = call i64 " ++ yName ++ "(i64 %mid)\nret i64 %res\n";
        return ("i64", "i64")
        -- translateValue x >>= callReturn "%arg"
        -- translateValue y >>= callReturn "rax"
    )
translateValue (TypeWithUnaryArrowOperator ArrowFirst _ value) =
  translateAnonymousFunction
    ( do
        name <- translateValue value
        getFirstSecondPointers "%arg" "%first.old" "%second.old"
        pointerDereference "%v1.old" "%first.old"
        pointerDereference "%v2" "%second.old"
        write $ "%v1.new = call i64 " ++ name ++ "(i64 %v1.old)\n"
        createNewPair "%res" "%first.new" "%second.new"
        store "%first.new" "%v1.new"
        store "%second.new" "%v2"
        write "ret ptr %res\n"
        return ("ptr", "ptr")
    )
translateValue (TypeWithUnaryArrowOperator ArrowSecond _ value) =
  translateAnonymousFunction
    ( do
        name <- translateValue value
        getFirstSecondPointers "%arg" "%first.old" "%second.old"
        pointerDereference "%v1" "%first.old"
        pointerDereference "%v2.old" "%second.old"
        write $ "%v2.new = call i64 " ++ name ++ "(i64 %v2.old)\n"
        createNewPair "%res" "%first.new" "%second.new"
        store "%first.new" "%v1"
        store "%second.new" "%v2.new"
        write "ret ptr %res\n"
        return ("ptr", "ptr")
    )
translateValue (TypeWithBinaryArrowOperator TripleAsterisks _ x y) =
  translateAnonymousFunction
    ( do
        getFirstSecondPointers "%arg" "%first.old" "%second.old"
        pointerDereference "%v1.old" "%first.old"
        pointerDereference "%v2.old" "%second.old"
        xName <- translateValue x
        write $ "%v1.new = call i64 " ++ xName ++ "(i64 %v1.old)\n"
        yName <- translateValue y
        write $ "%v2.new = call i64 " ++ yName ++ "(i64 %v2.old)\n"
        createNewPair "%res" "%first.new" "%second.new"
        store "%first.new" "%v1.new"
        store "%second.new" "%v2.new"
        write "ret ptr %res\n"
        return ("ptr", "ptr")
    )
translateValue (TypeWithBinaryArrowOperator TripleAnd _ x y) =
  translateAnonymousFunction
    ( do
        write "%arg.new = call ptr @double(i64 %arg)"
        getFirstSecondPointers "%arg.new" "%first.old" "%second.old"
        pointerDereference "%v1.old" "%first.old"
        pointerDereference "%v2.old" "%second.old"
        xName <- translateValue x
        write $ "%v1.new = call i64 " ++ xName ++ "(i64 %v1.old)\n"
        yName <- translateValue y
        write $ "%v2.new = call i64 " ++ yName ++ "(i64 %v2.old)\n"
        createNewPair "%res" "%first.new" "%second.new"
        store "%first.new" "%v1.new"
        store "%second.new" "%v2.new"
        write "ret ptr %res\n"
        return ("i64", "ptr")
    )
translateValue (TypeWithUnaryArrowOperator ArrowLeft _ value) =
  translateAnonymousFunction
    ( do
        getFirstSecondPointers "%arg" "%first.old" "%second.old"
        pointerDereference "%choice" "%first.old"
        pointerDereference "%v.old" "%second.old"
        lableLeft <- ("left" ++) . show <$> getNewLableIndex
        lableDone <- ("done" ++) . show <$> getNewLableIndex
        write "%choice.bool = icmp eq i64 %choice, 0\n"
        write $ "br i1 %choice.bool, label %" ++ lableLeft ++ ", label %" ++ lableDone ++ "\n"
        write $ lableLeft ++ ":\n"
        name <- translateValue value
        write $ "%v.new = call i64 " ++ name ++ "(i64 %v.old)\n"
        createNewPair "%res" "%first.new" "%second.new"
        store "%first.new" "%choice"
        store "%second.new" "%v.new"
        write "ret ptr %res\n"
        write $ lableDone ++ ":\nret ptr %arg\n"
        return ("ptr", "ptr")
    )
translateValue (TypeWithUnaryArrowOperator ArrowRight _ value) =
  translateAnonymousFunction
    ( do
        getFirstSecondPointers "%arg" "%first.old" "%second.old"
        pointerDereference "%choice" "%first.old"
        pointerDereference "%v.old" "%second.old"
        lableRight <- ("right" ++) . show <$> getNewLableIndex
        lableDone <- ("done" ++) . show <$> getNewLableIndex
        write "%choice.bool = icmp eq i64 %choice, 0\n"
        write $ "br i1 %choice.bool, label %" ++ lableDone ++ ", label %" ++ lableRight ++ "\n"
        write $ lableRight ++ ":\n"
        name <- translateValue value
        write $ "%v.new = call i64 " ++ name ++ "(i64 %v.old)\n"
        createNewPair "%res" "%first.new" "%second.new"
        store "%first.new" "%choice"
        store "%second.new" "%v.new"
        write "ret ptr %res\n"
        write $ lableDone ++ ":\nret ptr %arg\n"
        return ("ptr", "ptr")
    )
translateValue (TypeWithBinaryArrowOperator TriplePlus _ x y) =
  translateAnonymousFunction
    ( do
        getFirstSecondPointers "%arg" "%first.old" "%second.old"
        pointerDereference "%choice" "%first.old"
        pointerDereference "%v.old" "%second.old"
        lableLeft <- ("right" ++) . show <$> getNewLableIndex
        lableRight <- ("done" ++) . show <$> getNewLableIndex
        createNewPair "%res" "%first.new" "%second.new"
        store "%first.new" "%choice"
        write "%choice.bool = icmp eq i64 %choice, 0\n"
        write $ "br i1 %choice.bool, label %" ++ lableLeft ++ ", label %" ++ lableRight ++ "\n"
        write $ lableLeft ++ ":\n"
        xName <- translateValue x
        write $ "%v.left.new = call i64 " ++ xName ++ "(i64 %v.old)\n"
        store "%second.new" "%v.left.new"
        write "ret ptr %res\n"
        write $ lableRight ++ ":\n"
        yName <- translateValue y
        write $ "%v.right.new = call i64 " ++ yName ++ "(i64 %v.old)\n"
        store "%second.new" "%v.right.new"
        write "ret ptr %res\n"
        return ("ptr", "ptr")
    )
translateValue (TypeWithBinaryArrowOperator TripleBar _ x y) =
  translateAnonymousFunction
    ( do
        getFirstSecondPointers "%arg" "%first.old" "%second.old"
        pointerDereference "%choice" "%first.old"
        pointerDereference "%v.old" "%second.old"
        lableLeft <- ("right" ++) . show <$> getNewLableIndex
        lableRight <- ("done" ++) . show <$> getNewLableIndex
        write "%choice.bool = icmp eq i64 %choice, 0\n"
        write $ "br i1 %choice.bool, label %" ++ lableLeft ++ ", label %" ++ lableRight ++ "\n"
        write $ lableLeft ++ ":\n"
        xName <- translateValue x
        write $ "%v.left.new = call i64 " ++ xName ++ "(i64 %v.old)\n"
        write "ret i64 %v.left.new\n"
        write $ lableRight ++ ":\n"
        yName <- translateValue y
        write $ "%v.right.new = call i64 " ++ yName ++ "(i64 %v.old)\n"
        write "ret i64 %v.right.new\n"
        return ("ptr", "i64")
    )
translateValue (TypeWithUndefined _ sourcePos) = translateAnonymousFunction (write "unreachable" $> ("i64", "i64"))
translateValue (TypeWithUntranslateable _) = return "@id"

getFirstSecondPointers :: String -> String -> String -> ParserWithDoubleState TranslationState Program ()
getFirstSecondPointers pair first second = write $ first ++ " = getelementptr {i64, i64}, ptr " ++ pair ++ ", i64 0, i32 0\n" ++ second ++ " = getelementptr {i64, i64}, ptr " ++ pair ++ ", i64 0, i32 1\n"

createNewPair :: String -> String -> String -> ParserWithDoubleState TranslationState Program ()
createNewPair pair first second = write (pair ++ " = call {i64, i64}* @malloc(i64 16)\n") >> getFirstSecondPointers pair first second

pointerDereference :: String -> String -> ParserWithDoubleState TranslationState Program ()
pointerDereference result pointer = write $ result ++ " = load i64, ptr " ++ pointer ++ "\n"

store :: String -> String -> ParserWithDoubleState TranslationState Program ()
store pointer v = write $ "store i64 " ++ v ++ ", i64* " ++ pointer ++ "\n"

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

addDataSection :: ParserWithDoubleState TranslationState Program DataSection -> ParserWithDoubleState TranslationState Program String
addDataSection f = do
  index <- gets stateDataIndex
  modify $ \state -> state {stateDataIndex = index + 1}
  value <- f
  maybeIndex <- gets $ fmap fst . find ((== value) . snd) . stateDataSection
  createConstantName <$> case maybeIndex of
    (Just i) -> return i
    Nothing -> do
      modify $ \state -> state {stateDataSection = stateDataSection state ++ [(index, value)]}
      return index

addDataSectionPreview :: Maybe DataSection -> ParserWithDoubleState TranslationState Program String
addDataSectionPreview value = do
  maybeIndex <- gets $ fmap fst . find ((== value) . Just . snd) . stateDataSection
  createConstantName <$> case maybeIndex of
    (Just i) -> return i
    Nothing -> gets stateDataIndex

endFunction :: String -> (String, String) -> ParserWithDoubleState TranslationState Program ()
endFunction name (argType, returnType) = do
  modify $ \state -> state {stateText = "", stateTextSection = stateTextSection state ++ ["define " ++ returnType ++ " " ++ name ++ "(" ++ argType ++ " %arg) {\n" ++ stateText state ++ "}\n"]}

dataSectionToText :: Int -> DataSection -> String
dataSectionToText index (IntData value) = createConstantName index ++ " = private constant i64 " ++ show value
dataSectionToText index (FloatData value) = createConstantName index ++ " = private constant double " ++ show value
dataSectionToText index (ProductData xName yName) = createConstantName index ++ " = private constant {" ++ getLlvmeType xName ++ ", " ++ getLlvmeType yName ++ "} {" ++ getLlvmeType xName ++ " " ++ xName ++ ", " ++ getLlvmeType yName ++ " " ++ yName ++ "}"
dataSectionToText index (SumData boolChoice innerName) = createConstantName index ++ " = private constant {i64, " ++ getLlvmeType innerName ++ "} {i64 " ++ (if boolChoice then "1" else "0") ++ ", " ++ getLlvmeType innerName ++ " " ++ innerName ++ "}"

returnValue :: String -> ParserWithDoubleState TranslationState Program String
returnValue name = write ("ret " ++ getLlvmeType name ++ " " ++ name ++ "\n") >> return (getLlvmeType name)

isPtr :: String -> Bool
isPtr name = head name == '@' || head name == '%'

getLlvmeType :: String -> String
getLlvmeType name 
  | isPtr name = "ptr"
  | '.' `elem` name = "double"
  | otherwise = "i64"

callReturn :: String -> String -> ParserWithDoubleState TranslationState Program ()
callReturn argument name = write $ "push qword " ++ argument ++ "\ncall " ++ name ++ "\nadd rsp, 8\n"

data CompilerDefinition = CompilerDefinition {compilerDefinitionAsmName :: String, compilerDefinitionInstanceType :: Maybe ReferentialType, compilerDefinitionName :: Name}

compilerDefinitions :: [CompilerDefinition]
compilerDefinitions =
  [ CompilerDefinition "@putChar" Nothing "putChar",
    CompilerDefinition "@readChar" Nothing "readChar",
    CompilerDefinition "@fst" Nothing "fst",
    CompilerDefinition "@snd" Nothing "snd",
    CompilerDefinition "@double" Nothing "double",
    CompilerDefinition "@id" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "id",
    CompilerDefinition "@id" (Just $ idArrowType 0) "id",
    CompilerDefinition "@app" (Just $ idArrowType 0) "app",
    CompilerDefinition "@addInt" (Just $ ReferentialType (AliasReference "Int" []) []) "+",
    CompilerDefinition "@subInt" (Just $ ReferentialType (AliasReference "Int" []) []) "-",
    CompilerDefinition "@mulInt" (Just $ ReferentialType (AliasReference "Int" []) []) "*",
    CompilerDefinition "@divInt" (Just $ ReferentialType (AliasReference "Int" []) []) "/",
    CompilerDefinition "@absInt" (Just $ ReferentialType (AliasReference "Int" []) []) "abs",
    CompilerDefinition "@negInt" (Just $ ReferentialType (AliasReference "Int" []) []) "neg",
    CompilerDefinition "@addFloat" (Just $ ReferentialType (AliasReference "Float" []) []) "+",
    CompilerDefinition "@subFloat" (Just $ ReferentialType (AliasReference "Float" []) []) "-",
    CompilerDefinition "@mulFloat" (Just $ ReferentialType (AliasReference "Float" []) []) "*",
    CompilerDefinition "@divFloat" (Just $ ReferentialType (AliasReference "Float" []) []) "/",
    CompilerDefinition "@absFloat" (Just $ ReferentialType (AliasReference "Float" []) []) "abs",
    CompilerDefinition "@negFloat" (Just $ ReferentialType (AliasReference "Float" []) []) "neg",
    CompilerDefinition "@mod" Nothing "%",
    CompilerDefinition "@not" Nothing "not",
    CompilerDefinition "@and" Nothing "&&",
    CompilerDefinition "@or" Nothing "||",
    CompilerDefinition "@and" Nothing "&",
    CompilerDefinition "@or" Nothing "|",
    CompilerDefinition "@xor" Nothing "^",
    CompilerDefinition "@l" Nothing "l",
    CompilerDefinition "@r" Nothing "r",
    CompilerDefinition "@id" Nothing "chr",
    CompilerDefinition "@id" Nothing "ord",
    CompilerDefinition "@id" Nothing "choice",
    CompilerDefinition "@eq" (Just $ ReferentialType (AliasReference "Bool" []) []) "==",
    CompilerDefinition "@eq" (Just $ ReferentialType (AliasReference "Char" []) []) "==",
    CompilerDefinition "@eq" (Just $ ReferentialType (AliasReference "Int" []) []) "==",
    CompilerDefinition "@lessInt" (Just $ ReferentialType (AliasReference "Int" []) []) "<",
    CompilerDefinition "@greaterInt" (Just $ ReferentialType (AliasReference "Int" []) []) ">",
    CompilerDefinition "@lessEqInt" (Just $ ReferentialType (AliasReference "Int" []) []) "<=",
    CompilerDefinition "@greaterEqInt" (Just $ ReferentialType (AliasReference "Int" []) []) ">=",
    CompilerDefinition "@eqFloat" (Just $ ReferentialType (AliasReference "Float" []) []) "==",
    CompilerDefinition "@lessFloat" (Just $ ReferentialType (AliasReference "Float" []) []) "<",
    CompilerDefinition "@greaterFloat" (Just $ ReferentialType (AliasReference "Float" []) []) ">",
    CompilerDefinition "@lessEqFloat" (Just $ ReferentialType (AliasReference "Float" []) []) "<=",
    CompilerDefinition "@greaterEqFloat" (Just $ ReferentialType (AliasReference "Float" []) []) ">=",
    CompilerDefinition "@floatToInt" Nothing "float2Int",
    CompilerDefinition "@first" (Just $ idArrowType 0) "first",
    CompilerDefinition "@second" (Just $ idArrowType 0) "second",
    CompilerDefinition "@composition" (Just $ idArrowType 0) ">>>",
    CompilerDefinition "@tripleAsterisk" (Just $ idArrowType 0) "***",
    CompilerDefinition "@left" (Just $ idArrowType 0) "left",
    CompilerDefinition "@right" (Just $ idArrowType 0) "right",
    CompilerDefinition "@tripleAnd" (Just $ idArrowType 0) "&&&",
    CompilerDefinition "@triplePlus" (Just $ idArrowType 0) "+++",
    CompilerDefinition "@tripleBar" (Just $ idArrowType 0) "|||",
    CompilerDefinition "@app" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "app",
    CompilerDefinition "@first" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "first",
    CompilerDefinition "@second" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "second",
    CompilerDefinition "@composition" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) ">>>",
    CompilerDefinition "@tripleAsterisk" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "***",
    CompilerDefinition "@left" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "left",
    CompilerDefinition "@right" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "right",
    CompilerDefinition "@tripleAnd" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "&&&",
    CompilerDefinition "@triplePlus" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "+++",
    CompilerDefinition "@tripleBar" (Just $ ReferentialType (AliasReference (Name 0 "IO") []) []) "|||",
    CompilerDefinition "@swap" Nothing "swap",
    CompilerDefinition "@swapChoice" Nothing "swapChoice",
    CompilerDefinition "@reorderToFront" Nothing "reorderToFront",
    CompilerDefinition "@reorderToBack" Nothing "reorderToBack",
    CompilerDefinition "@includeLeft" Nothing "includeLeft",
    CompilerDefinition "@includeRight" Nothing "includeRight"
  ]

createFunctionName :: ParserWithDoubleState TranslationState Program String
createFunctionName = do
  index <- gets stateFunctionIndex
  modify $ \state -> state {stateFunctionIndex = index + 1}
  return $ "@fun" ++ show index

createFunctionNamePrewiev :: ParserWithDoubleState TranslationState Program String
createFunctionNamePrewiev = do
  index <- gets stateFunctionIndex
  return $ "@fun" ++ show index

createConstantName :: Int -> String
createConstantName index = "@const" ++ show index

getNewLableIndex :: ParserWithDoubleState TranslationState Program Int
getNewLableIndex = do
  index <- gets stateLabelIndex
  modify $ \state -> state {stateLabelIndex = index + 1}
  return index

createRegisterName :: ParserWithDoubleState TranslationState Program String
createRegisterName = do
  index <- gets stateRegisterIndex
  modify $ \state -> state {stateRegisterIndex= index + 1}
  return $ "%r" ++ show index

preludeLlvm :: String
preludeLlvm = $(embedStringFile "app/Prelude/Prelude.ll")
