module Tests.SchemaGeneration
  where

import Test.QuickCheck
import Control.Monad (replicateM)

-- Definitions

newtype FieldName = FieldName String
instance Show (FieldName) where
  show (FieldName fn) = fn

newtype StructName = StructName String
instance Show (StructName) where
  show (StructName fn) = fn

data Field = FieldDef FieldName Int FieldType
           | StructDef StructName [Field]

data BuiltIn = Void
             | Bool
             | Int8 | Int16 | Int32 | Int64
             | UInt8 | UInt16 | UInt32 | UInt64
             | Float32 | Float64
             | Text
             | Data
  deriving (Show, Enum)

data FieldType = BasicType BuiltIn
               | ListType FieldType
               | StructType StructName
instance Show (FieldType) where
  show (BasicType bi) = show bi
  show (ListType ft) = "List(" ++ (show ft) ++ ")"
  show (StructType sn) = show sn

instance Show (Field) where
  show (FieldDef name order entryType) =
    (show name) ++ " @" ++ (show order) ++ " :" ++
    (show entryType) ++ ";\n"
  show (StructDef name content) =
    "struct " ++ (show name) ++ " {\n"
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

genEnumerated :: Gen a -> Gen [a]
genEnumerated gen = sized $ \n -> do
  k <- choose (1, 1 `max` n)
  replicateM k gen

-- Field types

-- need to enumerate each field; this will be performed during struct
-- generation where the number of fields is known (numberDefs)
genFieldDef :: [FieldType] -> Int -> Gen Field
genFieldDef structTypes order = do
  fieldName <- do
    str <- listOf1 genSafeLCChar
    return $ FieldName (str ++ show order)
  fieldType <- elements ((map BasicType [Bool ..]) ++ structTypes)
  
  return $ FieldDef fieldName order fieldType

-- Struct type

-- like fields, we enumerate each struct during generation for uniqueness
genStructDef :: Int -> Int -> Gen Field
genStructDef depth order = do
  -- generate the struct's name
  structName <- do
    fc <- genSafeUCChar
    rest <- listOf genSafeLCChar
    return $ StructName ((fc:rest) ++ (show order))

  -- generate the nested structs
  structDefIndices <- if depth == 0
                      then pure []
                      else (\n -> [0..n]) <$> choose (0, 3)
  structDefs <- mapM (genStructDef (depth - 1)) structDefIndices

  -- extract the available struct types
  let structTypes = map (\(StructDef sn _) -> (StructType sn)) structDefs

  -- generate the fields using available struct types
  numFieldDefs <- sized (\n -> choose (1, 1 `max` n))
  fieldDefs <- mapM (genFieldDef structTypes) [0..numFieldDefs]

  return $ StructDef structName (fieldDefs ++ structDefs)

  -- Schema type

genSchema :: Gen Schema
genSchema = do
  id1st <- elements ['a'..'f']
  idrest <- vectorOf 15 genSafeHexChar
  content <- genStructDef 3 0 -- multiple structs make tests take too long
  return $ Schema (id1st:idrest) [content]

instance Arbitrary Schema where
  arbitrary = genSchema
