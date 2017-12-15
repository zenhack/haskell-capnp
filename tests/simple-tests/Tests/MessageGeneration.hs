{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Tests.MessageGeneration
    ( Message (..), genMessage
    ) where

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
    = Void
    | Bool Bool
    | Int Int
    | Float Float
    | Text String
    | Data BS.ByteString

instance Show BasicData where
    show (Int i) = show i
    show (Bool b) = show b
    show (Float f) = show f
    show (Text t) = t
    show (Data d) = show d

data MessageContent
    = BasicContent BasicData
    | ListContent [MessageContent]
    | StructContent [MessageItem]

joinStr :: String -> [String] -> String
joinStr sep [] = ""
joinStr sep (x:[]) = x
joinStr sep (x:xs) = x ++ sep ++ (joinStr sep xs)
instance Show MessageContent where
    show (BasicContent bc) = show bc
    show (ListContent lc) = "[" ++ (joinStr "," $ map show lc) ++ "]"
    show (StructContent sc) = "(" ++ (joinStr "," $ map show sc) ++ ")"

data MessageItem
    = MessageItem SG.FieldName MessageContent

instance Show MessageItem where
    show (MessageItem (SG.FieldName fn) mc) = fn ++ " = " ++ show mc

data Message
    = Message
        { msgSchema :: SG.Schema
        , msgItem   :: MessageItem
        }

instance Show Message where
    show (Message _ md) = show md

-- MessageItem types
genBasicContent :: SG.BuiltIn -> QC.Gen BasicData
genBasicContent bt = do
    return $ case bt of
        SG.Void    -> undefined
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

genMessageContent :: SG.FieldType -> QC.Gen MessageContent
genMessageContent ft = 
    case ft of
        SG.BasicType bi -> do
            bc <- genBasicContent bi
            return $ BasicContent bc
        SG.ListType it -> do
            lc <- genMessageContent it
            return $ ListContent [lc]
        SG.StructType (SG.StructName sn) -> do
            return $ BasicContent $ Text ("lookup: " ++ sn)
genMessageContent _ = undefined

genMessageItem :: SG.Field -> QC.Gen MessageItem
genMessageItem (SG.FieldDef fn _ ft) = do
    mc <- genMessageContent ft
    return $ MessageItem fn mc
genMessageItem (SG.StructDef (SG.StructName sn) fds) = do
    return $ MessageItem (SG.FieldName "new_struct_def") $ BasicContent $ Text sn

genFromSchema :: SG.Schema -> QC.Gen MessageItem
genFromSchema (SG.Schema sid sfs) = do
    items <- mapM genMessageItem sfs
    return $ head items


-- Message type
genMessage :: QC.Gen Message
genMessage = do
    msgSchema <- SG.genSchema
    msgItem <- genFromSchema msgSchema
    return $ Message msgSchema msgItem