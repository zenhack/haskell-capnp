{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Fmt
Description: Helpers for generating haskell code.

This module defines combinators for generating haskell code, on top of
wl-pprint.

We will expand this to cover more constructs, make it more type safe,
and so forth as we go.
-}
module Fmt where

import Text.PrettyPrint.Leijen.Text (Doc, hcat, vcat)

import qualified Text.PrettyPrint.Leijen.Text as PP

indent = PP.indent 4

-- | @'data_' typeCon dataCons derving_@ generates a @data@
-- declaration. @typeCon@ is the texst of the type constructor
-- *and type parameters*. @dataCons@ are the data constructors.
-- @deriving_@ is a list of type classes to go in the deriving
-- clause, if any.
data_ :: Doc -> [Doc] -> [Doc] -> Doc
data_ typeCon dataCons deriving_ = vcat
    [ hcat [ "data ", typeCon ]
    , indent $ vcat
        [ " = " <> vcat (PP.punctuate " |" dataCons)
        , case deriving_ of
            [] -> ""
            _  -> "deriving" <> PP.tupled deriving_
        ]
    ]
