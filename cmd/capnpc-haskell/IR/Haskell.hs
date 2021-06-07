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

    , DataArgs(..)
    , DataDecl(..)
    , DataVariant(..)
    , ClassDecl(..)
    , Decl(..)
    , Do(..)
    , Exp(..)
    , Export(..)
    , Import(..)
    , InstanceDef(..)
    , Module(..)
    , Pattern(..)
    , Type(..)
    , TypeAlias(..)
    , ValueDef(..)
    ) where

import Data.List (intercalate)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T

import qualified IR.Common as Common
import qualified IR.Name   as Name

-- | A Haskell module
data Module = Module
    { modName        :: [Name.UnQ]
    -- ^ The parts of the module path
    , modLangPragmas :: [T.Text]
    -- ^ The language extensions enabled in this module.
    , modExports     :: Maybe [Export]
    -- ^ The export list. If it is 'Nothing' that means there is no export
    -- list (and therefore everything is exported).
    , modDecls       :: [Decl]
    -- ^ The declarations in the module.
    , modImports     :: [Import]
    -- ^ Modules to import.
    }
    deriving(Show, Read, Eq)

data Export
    = ExportMod [Name.UnQ] -- module Foo.Bar
    | ExportGCtors Name.GlobalQ -- Foo.Bar.Baz(..)
    | ExportLCtors Name.LocalQ -- Baz(..)
    | ExportGName Name.GlobalQ -- Foo.Bar.Baz
    | ExportLName Name.LocalQ -- Baz
    deriving(Show, Read, Eq)

data Import
    = ImportAs -- import qualified Foo.Bar as Bar
        { importAs :: Name.UnQ
        , parts    :: [Name.UnQ]
        }
    | ImportQual -- import qualified Foo.Bar
        { parts :: [Name.UnQ]
        }
    | ImportAll -- import Foo.Bar
        { parts :: [Name.UnQ]
        }
    deriving(Show, Read, Eq)

-- | A declaration.
data Decl
    = DcData DataDecl
    | DcValue
        { typ :: Type
        , def :: ValueDef
        }
    | DcInstance
        { ctx  :: [Type]
        , typ  :: Type
        , defs :: [InstanceDef]
        }
    | DcTypeInstance Type Type
    | DcDeriveInstance
        { ctx :: [Type]
        , typ :: Type
        }
    | DcClass
        { ctx     :: [Type]
        , name    :: Name.LocalQ
        , params  :: [Name.UnQ]
        , funDeps :: [(T.Text, T.Text)]
        , decls   :: [ClassDecl]
        }
    deriving(Show, Read, Eq)

data DataDecl = Data
    { dataName     :: Name.UnQ
    -- ^ The name of the declared type.
    , typeArgs     :: [Type]
    , dataVariants :: [DataVariant]
    -- ^ The variants/data constructors for the type.
    , derives      :: [Name.UnQ]
    -- ^ A list of type classes to include in the deriving clause.
    , dataNewtype  :: !Bool
    -- ^ Whether the declarationis a "newtype" or "data" declaration.
    , dataInstance :: !Bool
    -- ^ If true, this is actually an instance of a data family, rather
    -- than a stand-alone type.
    }
    deriving(Show, Read, Eq)

data ClassDecl
    = CdValueDecl Name.UnQ Type
    | CdValueDef ValueDef
    | CdMinimal [Name.UnQ]
    -- ^ A MINIMAL pragma.
    deriving(Show, Read, Eq)

data InstanceDef
    = IdValue ValueDef
    | IdData DataDecl
    | IdType TypeAlias
    deriving(Show, Read, Eq)

data TypeAlias = TypeAlias Name.UnQ [Type] Type
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
    | ARec [(Name.UnQ, Type)]
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
    | TKindAnnotated Type Type
    | TString T.Text -- type-level string literal
    deriving(Show, Read, Eq)

data Exp
    = EVar T.Text
    | EApp Exp [Exp]
    | EFApp Exp [Exp]
    -- ^ "Functorial" application, i.e. f <$> x <*> y <*> z.
    | EGName Name.GlobalQ
    | ELName Name.LocalQ
    | EInt Integer
    | EDo [Do] Exp
    | EBind Exp Exp
    -- ^ A call to (>>=)
    | ETup [Exp]
    | EBytes LBS.ByteString
    | ECase Exp [(Pattern, Exp)]
    | ETypeAnno Exp Type
    | ELambda [Pattern] Exp
    | ERecord Exp [(Name.UnQ, Exp)]
    | ELabel Name.UnQ
    deriving(Show, Read, Eq)

data Do
    = DoBind Name.UnQ Exp
    | DoE Exp
    deriving(Show, Read, Eq)

data Pattern
    = PVar T.Text
    | PLCtor Name.LocalQ [Pattern]
    | PGCtor Name.GlobalQ [Pattern]
    | PInt Integer
    -- | Name{..}
    | PLRecordWildCard Name.LocalQ
    deriving(Show, Read, Eq)

-- | Get the file path for a module. For example, the module @Foo.Bar.Baz@ will
-- have a file path of @Foo/Bar/Baz.hs@.
modFilePath :: Module -> FilePath
modFilePath Module{modName} =
    intercalate "/" (map (T.unpack . Name.renderUnQ) modName) ++ ".hs"
