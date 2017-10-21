{-# LANGUAGE OverloadedStrings #-}
module Namespace
    ( NS
    , moduleName
    , moduleFile
    , fromId
    , subNS
    )
  where

import Data.Word

import Data.ByteString.UTF8 (toString)
import Data.String          (fromString)
import Data.List            (intersperse)
import Data.Monoid          (mconcat, (<>))
import Text.Printf          (printf)

import qualified Data.ByteString           as BS
import qualified Data.CapNProto.BasicTypes as BT

type BS = BS.ByteString

newtype NS = NS [BT.Text BS]
    deriving(Ord, Eq)

subNS :: NS -> BT.Text BS -> NS
subNS (NS ns) name = NS (name:ns)

moduleName :: NS -> BT.Text BS
moduleName (NS ns) = mconcat $
    "Schema.CapNProto.Reader." :
    reverse (intersperse "." ns)

moduleFile :: NS -> FilePath
moduleFile (NS ns) = asStr $ mconcat ( "Schema/CapNProto/Reader/"
                                     : reverse (intersperse "/" ns)
                                     ) <> ".hs"
  where
    asStr (BT.Text path) = toString path

fromId :: Word64 -> NS
fromId id = NS [fromString $ 'X' : printf "%08x" id]
