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

import qualified Tests.SchemaGeneration as SG

-- Definitions

data BuiltInData
    = Void
    | Bool Bool
    | Int Int
    | Float Float
    | Text String
    | Data BS.ByteString

instance Show BuiltInData where
    show (Int i) = show i
    show (Bool b) = show b
    show (Float f) = show f
    show (Text t) = t
    show (Data d) = show d

data MessageItem
    = BuiltInItem SG.FieldName BuiltInData
    | StructItem SG.StructName [MessageItem]

instance Show MessageItem where
    show (BuiltInItem (SG.FieldName fn) bid) = fn ++ " = " ++ show bid
    show (StructItem _ mis) = "(" ++ (join "," $ map show mis) ++ ")"
        where join sep [] = ""
              join sep (x:[]) = x
              join sep (x:xs) = x ++ sep ++ (join sep xs)

data Message
    = Message
        { msgSchema :: SG.Schema
        , msgData   :: MessageItem
        }

instance Show Message where
    show (Message _ md) = show md

-- Message type
genMessage :: QC.Gen Message
genMessage = do
    let sampleSchema = SG.Schema "aa6fda3b444d262c"
            [ SG.StructDef (SG.StructName "A")
                [ SG.FieldDef (SG.FieldName "a") 0 (SG.BasicType SG.UInt64)
                , SG.FieldDef (SG.FieldName "b") 1 (SG.BasicType SG.Bool)
                ]
            ]
    let sampleData = StructItem (SG.StructName "A")
            [ BuiltInItem (SG.FieldName "a") (Int 72)
            , BuiltInItem (SG.FieldName "b") (Bool True)
            ]
    return $ Message sampleSchema sampleData