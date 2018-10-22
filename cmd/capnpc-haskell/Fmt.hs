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
        [ case dataCons of
            (d:ds) -> vcat $
                ("= " <> d) : map ("| " <>) ds
            [] ->
                ""
        , case deriving_ of
            [] -> ""
            _  -> "deriving" <> PP.tupled deriving_
        ]
    ]

-- @'instance_' ctx typeCon defs@ defines an instance for @typeCon@
-- given the context @ctx@. @defs@ is the set of definitions in the
-- instance.
instance_ :: [Doc] -> Doc -> [Doc] -> Doc
instance_ ctx typeCon defs = vcat
    [ hcat
        [ "instance "
        , case ctx of
            []    -> ""
            [one] -> one <> " => "
            _     -> PP.tupled ctx <> " => "
        , typeCon
        , case defs of
            [] -> ""
            _  -> " where"
        ]
    , indent $ vcat defs
    ]
