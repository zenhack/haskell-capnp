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
    , Exp(..)
    , Do(..)
    , Pattern(..)
    , Decl(..)
    , ValueDef(..)
    , DataVariant(..)
    , DataArgs(..)
    ) where

import Data.List (intercalate)

import qualified Data.Text as T

import qualified IR.Common as Common
import qualified IR.Name   as Name

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
    = DcData
        { dataName     :: Name.UnQ
        -- ^ The name of the declared type.
        , dataVariants :: [DataVariant]
        -- ^ The variants/data constructors for the type.
        , derives      :: [Name.UnQ]
        -- ^ A list of type classes to include in the deriving clause.
        }
    | DcNewtype
        { dataName    :: Name.UnQ
        , typeArgs    :: [T.Text]
        , dataVariant :: DataVariant
        , derives     :: [Name.UnQ]
        }
    | DcValue
        { typ :: Type
        , def :: ValueDef
        }
    | DcInstance
        { ctx  :: [Type]
        , typ  :: Type
        , defs :: [ValueDef]
        }
    deriving(Show, Read, Eq)

data ValueDef = DfValue
    { name   :: Name.UnQ
    , value  :: Exp
    , params :: [Pattern]
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
    = APos [Type]
    -- we'll add records at some point.
    deriving(Show, Read, Eq)

data Type
    = TGName Name.GlobalQ
    | TLName Name.LocalQ
    | TVar T.Text
    | TApp Type [Type]
    | TFn [Type]
    | TCtx [Type] Type
    | TPrim Common.PrimWord
    | TUnit
    deriving(Show, Read, Eq)

data Exp
    = EApp Exp [Exp]
    | EGName Name.GlobalQ
    | ELName Name.LocalQ
    | EInt Integer
    | EDo [Do] Exp
    | ETup [Exp]
    deriving(Show, Read, Eq)

data Do
    = DoBind Name.UnQ Exp
    deriving(Show, Read, Eq)

data Pattern
    = PVar T.Text
    | PLCtor Name.LocalQ [Pattern]
    | PInt Integer
    deriving(Show, Read, Eq)

-- | Get the file path for a module. For example, the module @Foo.Bar.Baz@ will
-- have a file path of @Foo/Bar/Baz.hs@.
modFilePath :: Module -> FilePath
modFilePath Module{modName} =
    intercalate "/" (map (T.unpack . Name.renderUnQ) modName) ++ ".hs"
