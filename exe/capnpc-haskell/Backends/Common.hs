{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Backends.Common
Description: Bits of code generation common to both backends.
-}
module Backends.Common where

import IR

import Data.String (IsString(..))

import           Text.PrettyPrint.Leijen.Text (hcat, vcat)
import qualified Text.PrettyPrint.Leijen.Text as PP

-- | Format a primitive word type.
fmtPrimWord :: PrimWord -> PP.Doc
fmtPrimWord PrimInt{isSigned=True,size}  = "Int" <> fromString (show size)
fmtPrimWord PrimInt{isSigned=False,size} = "Word" <> fromString (show size)
fmtPrimWord PrimFloat32                  = "Float"
fmtPrimWord PrimFloat64                  = "Double"
fmtPrimWord PrimBool                     = "Bool"
