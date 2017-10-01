module Tests.SchemaGeneration
  where

import Test.QuickCheck

-- Definitions

newtype FieldName = FieldName { unwrapFieldName :: String }
  deriving Show

newtype StructName = StructName { unwrapStructName :: String }
  deriving Show

data Field = FieldDef FieldName Int FieldType
           | StructDef StructName [Field]

data FieldType = Void
               | Bool
               | Int8 | Int16
               | Text
               | Data
               | ListType FieldType
               | StructType StructName
instance Show (FieldType) where
  show Void = "Void"
  show Bool = "Bool"
  show Int8 = "Int8"
  show Int16 = "Int16"
  show Text = "Text"
  show Data = "Data"
  show (ListType ft) = "List(" ++ (show ft) ++ ")"
  show (StructType sn) = unwrapStructName sn

instance Show (Field) where
  show (FieldDef name order entryType) =
    (unwrapFieldName name) ++ " @" ++ (show order) ++ " :" ++
    (show entryType) ++ ";\n"
  show (StructDef name content) =
    "struct " ++ (unwrapStructName name) ++ " {\n"
    ++ (foldl (++) "" ["\t" ++ (show se) | se <- content])
    ++ "}\n\n"

data Schema = Schema
  { schemaId :: String
  , schemaContent :: [Field]
  }
instance Show (Schema) where
  show s = "@0x" ++ (schemaId s) ++ ";\n\n"
    ++ (foldr (++) "" [show st | st <- schemaContent s])

-- Helper generators

genSafeLCChar :: Gen Char
genSafeLCChar = elements ['a'..'z']

genSafeUCChar :: Gen Char
genSafeUCChar = elements ['A'..'Z']

genSafeHexChar :: Gen Char
genSafeHexChar = elements (['0'..'9'] ++ ['a'..'f'])

-- Field types

-- need to enumerate each field; this will be performed during struct
-- generation where the number of fields is known (numberDefs)
genFieldDef :: [FieldType] -> Gen (Int -> Field)
genFieldDef structTypes = do
  fieldName <- genFieldName
  fieldType <- genFieldType
  return $ \order -> FieldDef (appendFN fieldName order) order fieldType
  where
    genFieldName :: Gen FieldName
    genFieldName = FieldName <$> (listOf1 genSafeLCChar)

    genFieldType :: Gen FieldType
    genFieldType = elements ([Bool, Int8, Int16, Text, Data] ++ structTypes)
    
    appendFN fn o = FieldName ((unwrapFieldName fn) ++ (show o))

-- Struct type

-- like fields, we enumerate each struct during generation for uniqueness
genStructDef :: Gen (Int -> Field)
genStructDef = do
  structName <- genStructName
  numStructDefs <- frequency [(3, choose (0, 1)), (1, return 2)]
  nestedStructDefs <- vectorOf numStructDefs genStructDef
  let structDefs = numberDefs nestedStructDefs
  let structTypes = map (\(StructDef sn _) -> (StructType sn)) structDefs
  fieldDefs <- listOf1 $ genFieldDef structTypes
  return $ \order -> StructDef
                     (appendSN structName order)
                     ((numberDefs fieldDefs) ++ structDefs)
  where
    genStructName :: Gen StructName
    genStructName = do
      fc <- genSafeUCChar
      rest <- listOf genSafeLCChar
      return $ StructName (fc:rest)
    
    appendSN sn o = StructName ((unwrapStructName sn) ++ (show o))
    numberDefs fds = map (\(f, order) -> f order) (zip fds [0..])

  -- Schema type

genSchema :: Gen Schema
genSchema = do
  id1st <- elements ['a'..'f']
  idrest <- vectorOf 15 genSafeHexChar
  content <- genStructDef -- multiple structs make tests take too long
  return $ Schema (id1st:idrest) [content 0]
  where numberDefs fds = map (\(f, order) -> f order) (zip fds [0..])

instance Arbitrary Schema where
  arbitrary = genSchema
