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
    (unwrapFieldName name) ++ " @" ++ (show order) ++ " :" ++ (show entryType) ++ ";\n"
  show (StructDef name content) =
    "struct " ++ (unwrapStructName name) ++ " {\n"
    ++ (foldl (++) "\t" [show se | se <- content])
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

-- Field types

genFieldName :: Gen FieldName
genFieldName = FieldName <$> (listOf1 genSafeLCChar)

genBuiltInType :: Gen FieldType
genBuiltInType = do
  return Bool

genFieldDef :: Int -> Gen Field
genFieldDef order = do
  fn <- genFieldName
  ft <- genBuiltInType
  return $ FieldDef fn order ft

-- Struct types

genStructName :: Gen StructName
genStructName = do
    fc <- genSafeUCChar
    rest <- listOf genSafeLCChar
    return $ StructName (fc:rest)

genStructDef :: Gen Field
genStructDef = do
  sn <- genStructName
  content <- genFieldDef 0
  return $ StructDef sn [content]

  -- Schema type

genSchema :: Gen Schema
genSchema = do
    content <- listOf1 genStructDef
    return $ Schema "b356b28dc0d11f83" content

instance Arbitrary Schema where
  arbitrary = genSchema
