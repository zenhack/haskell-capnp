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
    = BoolData Bool
    | IntData Int
    | FloatData Float
    | TextData String
    | RawData BS.ByteString

instance Show BasicData where
    show (IntData i) = show i
    show (BoolData b) = if b then "true" else "false"
    show (FloatData f) = show f
    show (TextData t) = show t
    show (RawData d) = show d

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
genBool    = QC.elements [BoolData True, BoolData False]
genInt :: Int -> QC.Gen BasicData
genInt i   = do { x <- QC.elements [-2^(i-1) .. 2^(i-1)]; return $ IntData x }
genUInt :: Int -> QC.Gen BasicData
genUInt i  = do { x <- QC.elements [0 .. 2^i]; return $ IntData x }
genFloat :: Int -> QC.Gen BasicData
genFloat i = do { x <- QC.elements [0 .. 2^i]; return $ FloatData (x / 2) }
genText :: QC.Gen BasicData
genText    = do { x <- QC.listOf (QC.elements ['A'..'z']); return $ TextData x }
genData :: QC.Gen BasicData
genData    = do { TextData t <- genText ; return $ RawData (BSC8.pack t) }

genBasicContent :: SG.BuiltIn -> QC.Gen BasicData
genBasicContent bt = do
    let signedRange i = [-2^(i-1) .. 2^(i-1)]
    let unsignedRange i = [0 .. 2^i]
    content <- case bt of
        SG.Bool    -> genBool
        SG.Int8    -> genInt 8
        SG.Int16   -> genInt 16
        SG.Int32   -> genInt 32
        SG.Int64   -> genInt 64
        SG.UInt8   -> genUInt 8
        SG.UInt16  -> genUInt 16
        SG.UInt32  -> genUInt 32
        SG.UInt64  -> genUInt 64
        SG.Float32 -> genFloat 32
        SG.Float64 -> genFloat 64
        SG.Text    -> genText
        SG.Data    -> genData
    return content

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