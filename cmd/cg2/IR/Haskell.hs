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
--    #..), and I(zenhack) don't see a good way to do that with the libraries
--    I looked at.
module IR.Haskell
    ( Format(..)

    , modFilePath

    , Module(..)
    , Decl(..)
    , DataVariant(..)
    ) where

import Data.List                    (intercalate, intersperse)
import Text.PrettyPrint.Leijen.Text (hcat, vcat)

import qualified Data.Text                    as T
import qualified Text.PrettyPrint.Leijen.Text as PP

import qualified IR.Name as Name

-- | A Haskell module
data Module = Module
    { modName  :: [Name.UnQ]
    -- ^ The parts of the module path
    , modDecls :: [Decl]
    -- ^ The declarations in the module.
    }
    deriving(Show, Read)

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
    deriving(Show, Read)

-- | A data constructor
data DataVariant = DataVariant
    { dvCtorName :: Name.UnQ
    -- ^ The name of the constructor.
    }
    deriving(Show, Read)

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

instance Format Name.UnQ where
    format = PP.textStrict . Name.renderUnQ

instance Format Module where
    format Module{modName, modDecls} = vcat
        [ hcat
            [ "module "
            , PP.textStrict $ mconcat $ intersperse "." $ map Name.renderUnQ modName
            , " where"
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
        [ PP.textStrict $ Name.renderUnQ name, " = error \"TODO\"" ]

formatDerives :: [Name.UnQ] -> PP.Doc
formatDerives [] = ""
formatDerives ds = " deriving" <> PP.tupled (map format ds)

instance Format DataVariant where
    format DataVariant{dvCtorName} = format dvCtorName

-- | Indent the argument by four spaces.
indent :: PP.Doc -> PP.Doc
indent = PP.indent 4
