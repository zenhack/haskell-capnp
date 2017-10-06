module Tests.SchemaGeneration
  where

import           Control.Monad   (replicateM)
import           Data.Foldable   (foldl')
import qualified Test.QuickCheck as QC

-- Definitions

newtype FieldName
  = FieldName String

instance Show FieldName where
  show (FieldName fn) = fn

newtype StructName
  = StructName String

instance Show StructName where
  show (StructName fn) = fn

data Field
  = FieldDef FieldName Int FieldType
  | StructDef StructName [Field]

data BuiltIn
  = Void
  | Bool
  | Int8
  | Int16
  | Int32
  | Int64
  | UInt8
  | UInt16
  | UInt32
  | UInt64
  | Float32
  | Float64
  | Text
  | Data
  deriving (Show, Enum)

data FieldType
  = BasicType BuiltIn
  | ListType FieldType
  | StructType StructName

instance Show FieldType where
  show (BasicType bi)  = show bi
  show (ListType ft)   = "List(" ++ show ft ++ ")"
  show (StructType sn) = show sn

instance Show Field where
  show (FieldDef name order entryType) = concat
                                         [ show name, " @", show order, " :"
                                         , show entryType, ";\n"
                                         ]
  show (StructDef name content)        = concat
                                         [ "struct ", show name, " {\n"
                                         , concatMap (('\t':) . show) content
                                         , "}\n\n"
                                         ]

data Schema
  = Schema
    { schemaId      :: String
    , schemaContent :: [Field]
    }

instance Show Schema where
  show s = concat
           [ "@0x", schemaId s, ";\n\n"
           , concatMap show (schemaContent s)
           ]

-- Helper generators

genSafeLCChar :: QC.Gen Char
genSafeLCChar = QC.elements ['a'..'z']

genSafeUCChar :: QC.Gen Char
genSafeUCChar = QC.elements ['A'..'Z']

genSafeHexChar :: QC.Gen Char
genSafeHexChar = QC.elements (['0'..'9'] ++ ['a'..'f'])

-- Field types

-- need to enumerate each field; this will be performed during struct
-- generation where the number of fields is known (numberDefs)
genFieldDef :: [FieldType] -> Int -> QC.Gen Field
genFieldDef structTypes order = do
  fieldName <- do
    str <- QC.listOf1 genSafeLCChar
    return $ FieldName (str ++ show order)
  fieldType <- QC.elements ((map BasicType [Bool ..]) ++ structTypes)

  return $ FieldDef fieldName order fieldType

-- Struct type

-- like fields, we enumerate each struct during generation for uniqueness
genStructDef :: Int -> Int -> QC.Gen Field
genStructDef depth order = do
  -- generate the struct's name
  structName <- do
    fc <- genSafeUCChar
    rest <- QC.listOf genSafeLCChar
    return $ StructName ((fc:rest) ++ (show order))

  -- generate the nested structs
  structDefIndices <- if depth == 0
                      then pure []
                      else (\n -> [0..n]) <$> QC.choose (0, 3)
  structDefs <- mapM (genStructDef (depth - 1)) structDefIndices

  -- extract the available struct types
  let structTypes = map (\(StructDef sn _) -> (StructType sn)) structDefs

  -- generate the fields using available struct types
  numFieldDefs <- QC.sized (\n -> QC.choose (1, 1 `max` n))
  fieldDefs <- mapM (genFieldDef structTypes) [0..numFieldDefs]

  return $ StructDef structName (fieldDefs ++ structDefs)

  -- Schema type

genSchema :: QC.Gen Schema
genSchema = do
  id1st <- QC.elements ['a'..'f']
  idrest <- QC.vectorOf 15 genSafeHexChar
  content <- genStructDef 3 0 -- multiple structs make tests take too long
  return $ Schema (id1st:idrest) [content]
