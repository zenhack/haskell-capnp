{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module IR.Haskell where

import Data.List (intersperse)

import qualified Data.Text as T

import qualified IR.Name as Name

class Format a where
    format :: a -> T.Text

data Module = Module
    { modDecls :: [Decl]
    }
    deriving(Show, Read)

instance Format Module where
    format Module{modDecls} =
        mconcat $ intersperse "\n" (map format modDecls)

data Decl = DataDecl
    { dataName     :: Name.UnQ
    , dataVariants :: [DataVariant]
    , derives      :: [Name.UnQ]
    }
    deriving(Show, Read)

instance Format Decl where
    format DataDecl{dataName, dataVariants, derives} = mconcat
        [ "data "
        , Name.renderUnQ dataName
        , " = "
        , mconcat $ intersperse " | " (map format dataVariants)
        , " deriving("
        , mconcat $ intersperse ", " (map Name.renderUnQ derives)
        , ")"
        ]

data DataVariant = DataVariant
    { dvCtorName :: Name.UnQ
    }
    deriving(Show, Read)

instance Format DataVariant where
    format DataVariant{dvCtorName} = Name.renderUnQ dvCtorName
