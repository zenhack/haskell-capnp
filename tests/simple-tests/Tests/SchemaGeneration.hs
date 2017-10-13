{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Tests.SchemaGeneration
  ( Schema (..), genSchema
  ) where

import           Control.Monad   (replicateM)
import           Data.Foldable   (foldl')
import qualified Test.QuickCheck as QC

import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

import Control.Monad.State.Strict

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

newtype FieldGen a
  = FieldGen (StateT (NonEmpty (Int, Int)) QC.Gen a)
  deriving (Functor, Applicative, Monad)

liftGen :: QC.Gen a -> FieldGen a
liftGen m = FieldGen (lift m)

runFieldGen :: FieldGen a -> QC.Gen a
runFieldGen (FieldGen m) = fst <$> runStateT m ((0, 0) :| [])

pushFieldGen :: FieldGen ()
pushFieldGen = FieldGen $ modify (NE.cons (0, 0))

popFieldGen :: FieldGen ()
popFieldGen = FieldGen $ do
  original <- get
  case original of
    (x :| (y : rest)) -> put (y :| rest)
    (x :| [])         -> put (x :| [])

getStructOrder :: FieldGen Int
getStructOrder = FieldGen $ do
  current <- get
  let (result, _) = NE.head current
  case current of
    ((x, y) :| rest) -> put ((x + 1, y) :| rest)
  return result

getOrder :: FieldGen Int
getOrder = FieldGen $ do
  current <- get
  let (_, result) = NE.head current
  case current of
    ((x, y) :| rest) -> put ((x + 1, y + 1) :| rest)
  return result

-- Field types

-- need to enumerate each field; this will be performed during struct
-- generation where the number of fields is known (numberDefs)
genFieldDef :: [FieldType] -> FieldGen Field
genFieldDef structTypes = do
  order <- getOrder
  fieldName <- do
    str <- liftGen $ QC.listOf1 genSafeLCChar
    return $ FieldName (str ++ show order)
  fieldType <- liftGen $ QC.elements (map BasicType [Bool ..] ++ structTypes)
  return $ FieldDef fieldName order fieldType

-- Struct type

-- like fields, we enumerate each struct during generation for uniqueness
genStructDef :: Int -> FieldGen Field
genStructDef depth = do
  order <- getStructOrder

  pushFieldGen

  -- generate the struct's name
  structName <- do
    fc <- liftGen genSafeUCChar
    rest <- liftGen (QC.listOf genSafeLCChar)
    return $ StructName ((fc:rest) ++ show order)

  -- generate the nested structs
  structNum  <- if depth <= 0
                then pure 0
                else liftGen (QC.choose (0, 3))
  structDefs <- replicateM structNum (genStructDef (depth - 1))

  -- extract the available struct types
  let structTypes = map (\(StructDef sn _) -> (StructType sn)) structDefs

  -- generate the fields using available struct types
  fieldNum  <- liftGen (QC.sized (\n -> QC.choose (1, 1 `max` n)))
  fieldDefs <- replicateM fieldNum (genFieldDef structTypes)

  popFieldGen

  return $ StructDef structName (fieldDefs ++ structDefs)

  -- Schema type

genSchema :: QC.Gen Schema
genSchema = do
  id1st <- QC.elements ['a'..'f']
  idrest <- QC.vectorOf 15 genSafeHexChar
  -- multiple structs make tests take too long
  content <- runFieldGen (genStructDef 3)
  return $ Schema (id1st:idrest) [content]
