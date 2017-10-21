{-# LANGUAGE OverloadedStrings #-}
module Namespace
    ( NS
    , moduleName
    , moduleFile
    )
  where

import Data.ByteString.UTF8 (toString)
import Data.List            (intersperse)
import Data.Monoid          (mconcat, (<>))

import qualified Data.ByteString           as BS
import qualified Data.CapNProto.BasicTypes as BT

type BS = BS.ByteString

type NS = [BT.Text BS]

moduleName :: NS -> BT.Text BS
moduleName ns = mconcat $
    "Schema.CapNProto.Reader." :
    reverse (intersperse "." ns)

moduleFile :: NS -> FilePath
moduleFile ns = asStr $ mconcat ( "Schema/CapNproto/Reader/"
                                : reverse (intersperse "/" ns)
                                ) <> ".hs"
  where
    asStr (BT.Text path) = toString path
