{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
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

data Module = Module
    { modName  :: [Name.UnQ]
    , modDecls :: [Decl]
    }
    deriving(Show, Read)

data Decl = DataDecl
    { dataName     :: Name.UnQ
    , dataVariants :: [DataVariant]
    , derives      :: [Name.UnQ]
    }
    deriving(Show, Read)

data DataVariant = DataVariant
    { dvCtorName :: Name.UnQ
    }
    deriving(Show, Read)

modFilePath :: Module -> FilePath
modFilePath Module{modName} =
    intercalate "/" (map (T.unpack . Name.renderUnQ) modName) ++ ".hs"

indent :: PP.Doc -> PP.Doc
indent = PP.indent 4

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
            , case derives of
                [] -> ""
                _  -> "deriving" <> PP.tupled (map format derives)
            ]
        ]

instance Format DataVariant where
    format DataVariant{dvCtorName} = format dvCtorName
