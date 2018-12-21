{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
-- A last-leg intermediate form, before emitting actually Haskell source code.
--
-- This module contains a *simplified* Haskell AST; it only includes the
-- features we actually need. We roll our own for two reasons:
--
-- 1. Full Haskell ASTs are actually quite complex, and the data types exposed
--    by libraries for working with Haskell source reflect this. What we need
--    is much simpler.
-- 2. At some point we'll want to inject Haddock comments into the source (see
--    #32), and I(zenhack) don't see a good way to do that with the libraries
--    I looked at.
module IR.Haskell
    ( modFilePath

    , Module(..)
    , Import(..)
    , Type(..)
    , Decl(..)
    , DataVariant(..)
    , DataArgs(..)
    ) where

import Data.List (intercalate)

import qualified Data.Text as T

import IR.Common (PrimType)

import qualified IR.Name as Name

-- | A Haskell module
data Module = Module
    { modName    :: [Name.UnQ]
    -- ^ The parts of the module path
    , modDecls   :: [Decl]
    -- ^ The declarations in the module.
    , modImports :: [Import]
    -- ^ Modules to import.
    }
    deriving(Show, Read, Eq)

data Import = Import
    { importAs :: Name.UnQ
    , parts    :: [Name.UnQ]
    }
    deriving(Show, Read, Eq)

-- | A declaration.
data Decl
    = DataDecl
        { dataName     :: Name.UnQ
        -- ^ The name of the declared type.
        , dataVariants :: [DataVariant]
        -- ^ The variants/data constructors for the type.
        , derives      :: [Name.UnQ]
        -- ^ A list of type classes to include in the deriving clause.
        }
    | NewtypeDecl
        { dataName    :: Name.UnQ
        , typeArgs    :: [T.Text]
        , dataVariant :: DataVariant
        , derives     :: [Name.UnQ]
        }
    | ValueDecl
        { name :: Name.UnQ
        }
    deriving(Show, Read, Eq)

-- | A data constructor
data DataVariant = DataVariant
    { dvCtorName :: Name.UnQ
    -- ^ The name of the constructor.
    , dvArgs     :: DataArgs
    }
    deriving(Show, Read, Eq)

-- | Arguments to a data constructor
data DataArgs
    = PosArgs [Type]
    -- we'll add records at some point.
    deriving(Show, Read, Eq)

data Type
    = NamedType Name.GlobalQ
    | TypeVar T.Text
    | TypeApp Type [Type]
    | PrimType PrimType
    deriving(Show, Read, Eq)

-- | Get the file path for a module. For example, the module @Foo.Bar.Baz@ will
-- have a file path of @Foo/Bar/Baz.hs@.
modFilePath :: Module -> FilePath
modFilePath Module{modName} =
    intercalate "/" (map (T.unpack . Name.renderUnQ) modName) ++ ".hs"
