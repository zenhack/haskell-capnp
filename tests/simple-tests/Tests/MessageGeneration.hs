{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Tests.MessageGeneration
    ( Message (..), genMessage
    ) where

import qualified Data.Map as Map

import           Control.Monad   (replicateM)
import           Data.Foldable   (foldl')
import qualified Test.QuickCheck as QC

import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

import Control.Monad.State.Strict

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8

import qualified Tests.SchemaGeneration as SG

-- Definitions

data BasicData
    = Bool Bool
    | Int Int
    | Float Float
    | Text String
    | Data BS.ByteString

instance Show BasicData where
    show (Int i) = show i
    show (Bool b) = if b then "true" else "false"
    show (Float f) = show f
    show (Text t) = show t
    show (Data d) = show d

data MessageContent
    = BasicContent BasicData
    | ListContent [MessageContent]
    | StructContent [MessageItem]

filterBlank [] = []
filterBlank ("":xs) = filterBlank xs
filterBlank (x:xs) = x:(filterBlank xs)
joinStr :: String -> [String] -> String
joinStr sep [] = ""
joinStr sep (x:[]) = x
joinStr sep (x:xs) = x ++ sep ++ (joinStr sep xs)
instance Show MessageContent where
    show (BasicContent bc) = show bc
    show (ListContent lc) = "[" ++ (joinStr ", " (filterBlank $ map show lc)) ++ "]"
    show (StructContent sc) = "(" ++ (joinStr ", " (filterBlank $ map show sc)) ++ ")"

data MessageItem
    = BlankItem
    | MessageItem SG.FieldName MessageContent

instance Show MessageItem where
    show (MessageItem (SG.FieldName fn) mc) = fn ++ " = " ++ show mc
    show BlankItem = ""

data Message
    = Message
        { msgSchema  :: SG.Schema
        , msgType    :: String
        , msgContent :: MessageContent
        }

instance Show Message where
    show (Message s mt mc) = show mc

-- MessageItem types
genBasicContent :: SG.BuiltIn -> QC.Gen BasicData
genBasicContent bt = do
    return $ case bt of
        SG.Bool    -> Bool False
        SG.Int8    -> Int 0
        SG.Int16   -> Int 0
        SG.Int32   -> Int 0
        SG.Int64   -> Int 0
        SG.UInt8   -> Int 0
        SG.UInt16  -> Int 0
        SG.UInt32  -> Int 0
        SG.UInt64  -> Int 0
        SG.Float32 -> Float 0.0
        SG.Float64 -> Float 0.0
        SG.Text    -> Text "text"
        SG.Data    -> Data (BSC8.pack "data")

newtype StructTable = StructTable (Map.Map String [SG.Field])

genMessageContent :: SG.FieldType -> StructTable -> QC.Gen MessageContent
genMessageContent ft (StructTable st) = 
    case ft of
        SG.BasicType bi -> do
            bc <- genBasicContent bi
            return $ BasicContent bc
        SG.ListType it -> do
            lc <- genMessageContent it (StructTable st)
            return $ ListContent [lc]
        SG.StructType (SG.StructName sn) -> do
            let Just sfs = Map.lookup sn st
            msgContent <- mapM (\ f -> genMessageItem f (StructTable st)) sfs
            return $ StructContent msgContent

genMessageItem :: SG.Field -> StructTable -> QC.Gen MessageItem
genMessageItem (SG.FieldDef fn _ ft) st = do
    mc <- genMessageContent ft st
    return $ MessageItem fn mc
genMessageItem _ _ = return BlankItem

genStructTable :: [SG.Field] -> StructTable
genStructTable sfs = aux sfs Map.empty
    where aux [] st = StructTable st
          aux ((SG.StructDef (SG.StructName sn) content):ss) st =
            let StructTable st' = aux content st
                st'' = Map.insert sn content st' in
                aux ss st''
          aux (_:ss) st = aux ss st

-- Message type
genMessage :: QC.Gen Message
genMessage = do
    msgSchema@(SG.Schema sid sfs) <- SG.genSchema
    let StructTable st = genStructTable sfs
    let SG.StructDef (SG.StructName structName) _ = head sfs
    let Just sfs = Map.lookup structName st
    msgContent <- mapM (\ f -> genMessageItem f (StructTable st)) sfs
    return $ Message msgSchema structName (StructContent msgContent)