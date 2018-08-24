{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-|
Module: Backends.Common
Description: Bits of code generation common to both backends.
-}
module Backends.Common where

import Data.Monoid ((<>))
import Data.String (IsString(..))

import qualified Text.PrettyPrint.Leijen.Text as PP

import IR

-- | Format a primitive word type.
fmtPrimWord :: PrimWord -> PP.Doc
fmtPrimWord PrimInt{isSigned=True,size}  = "Int" <> fromString (show size)
fmtPrimWord PrimInt{isSigned=False,size} = "Word" <> fromString (show size)
fmtPrimWord PrimFloat32                  = "Float"
fmtPrimWord PrimFloat64                  = "Double"
fmtPrimWord PrimBool                     = "Bool"

-- | Return the size in bits of a type that belongs in the data section of a struct.
dataFieldSize :: WordType -> Int
dataFieldSize fieldType = case fieldType of
    EnumType _           -> 16
    PrimWord PrimInt{..} -> size
    PrimWord PrimFloat32 -> 32
    PrimWord PrimFloat64 -> 64
    PrimWord PrimBool    -> 1
