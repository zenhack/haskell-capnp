{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
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
    ( Format(..)

    , modFilePath

    , Module(..)
    , Decl(..)
    , DataVariant(..)
    ) where

import Data.List                    (intercalate, intersperse)
import Data.String                  (fromString)
import Text.PrettyPrint.Leijen.Text (hcat, vcat)

import qualified Data.Text                    as T
import qualified Text.PrettyPrint.Leijen.Text as PP

import IR.Common (IntType(..), PrimType(..), Sign(..), sizeBits)

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
    }
    deriving(Show, Read, Eq)

-- | Get the file path for a module. For example, the module @Foo.Bar.Baz@ will
-- have a file path of @Foo/Bar/Baz.hs@.
modFilePath :: Module -> FilePath
modFilePath Module{modName} =
    intercalate "/" (map (T.unpack . Name.renderUnQ) modName) ++ ".hs"

-- | Types which can be rendered as haskell source code.
--
-- Note well: while format returns a Doc, it is not safe to render it using
-- wl-pprint's "compact" output; we rely on newline significance in some ways
-- without enforcing it.
--
-- It would be nice to fix this, but given that we don't currently expose this
-- via the library and only ever render it in the one place (in main), it isn't
-- a huge priority.
class Format a where
    format :: a -> PP.Doc

instance Format Module where
    format Module{modName, modDecls, modImports} = vcat
        [ hcat
            [ "module "
            , PP.textStrict $ mconcat $ intersperse "." $ map Name.renderUnQ modName
            , " where"
            ]
        , vcat $ map format modImports
        -- We import many things, including the prelude, qualified under the
        -- "Std_" namespace, so that they don't collide with names in the
        -- generated code; see issue #58.
        , vcat $ map format
            [ Import { importAs = "Std_", parts = ["Prelude"] }
            , Import { importAs = "Std_", parts = ["Data", "Word"] }
            , Import { importAs = "Std_", parts = ["Data", "Int"] }
            ]
        , vcat $ map format modDecls
        ]

instance Format Decl where
    format DataDecl{dataName, dataVariants, derives} = vcat
        [ hcat [ "data ", format dataName ]
        , indent $ vcat
            [ case dataVariants of
                (d:ds) -> vcat $ ("= " <> format d) : map (("| " <>) . format) ds
                []     -> ""
            , formatDerives derives
            ]
        ]
    format NewtypeDecl{dataName, dataVariant, derives} = vcat
        [ hcat [ "newtype ", format dataName ]
        , indent $ vcat
            [ hcat [ "= ", format dataVariant ]
            , formatDerives derives
            ]
        ]
    format ValueDecl{name} = hcat
        [ format name, " = error \"TODO\"" ]

formatDerives :: [Name.UnQ] -> PP.Doc
formatDerives [] = ""
formatDerives ds = " deriving" <> PP.tupled (map format ds)

instance Format Name.UnQ where
    format = PP.textStrict . Name.renderUnQ

instance Format DataVariant where
    format DataVariant{dvCtorName} = format dvCtorName

instance Format Import where
    format Import{importAs, parts} = hcat
        [ "import qualified "
        , mconcat $ intersperse "." (map format parts)
        , " as "
        , format importAs
        ]

instance Format PrimType where
    format PTyVoid = "()"
    format PTyBool = "Std_.Bool"
    format (PTyInt (IntType sign sz)) =
        let szDoc = fromString $ show $ sizeBits sz
            typePrefix = case sign of
                Signed   -> "Int"
                Unsigned -> "Word"
        in
        "Std_." <> typePrefix <> szDoc
    format PTyFloat32 = "Std_.Float"
    format PTyFloat64 = "Std_.Double"

-- | Indent the argument by four spaces.
indent :: PP.Doc -> PP.Doc
indent = PP.indent 4
