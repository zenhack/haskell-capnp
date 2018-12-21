{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Trans.HaskellToText (moduleToText) where

import Data.List                    (intersperse)
import Data.String                  (fromString)
import Text.PrettyPrint.Leijen.Text (hcat, vcat)

import qualified Text.PrettyPrint.Leijen.Text as PP

import IR.Common (IntType(..), PrimType(..), Sign(..), sizeBits)

import qualified IR.Haskell as Haskell
import qualified IR.Name    as Name

moduleToText :: Haskell.Module -> PP.Doc
moduleToText = format

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

instance Format Haskell.Module where
    format Haskell.Module{modName, modDecls, modImports} = vcat
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
            [ Haskell.Import { importAs = "Std_", parts = ["Prelude"] }
            , Haskell.Import { importAs = "Std_", parts = ["Data", "Word"] }
            , Haskell.Import { importAs = "Std_", parts = ["Data", "Int"] }
            ]
        , vcat $ map format modDecls
        ]

instance Format Haskell.Decl where
    format Haskell.DataDecl{dataName, dataVariants, derives} = vcat
        [ hcat [ "data ", format dataName ]
        , indent $ vcat
            [ case dataVariants of
                (d:ds) -> vcat $ ("= " <> format d) : map (("| " <>) . format) ds
                []     -> ""
            , formatDerives derives
            ]
        ]
    format Haskell.NewtypeDecl{dataName, dataVariant, derives} = vcat
        [ hcat [ "newtype ", format dataName ]
        , indent $ vcat
            [ hcat [ "= ", format dataVariant ]
            , formatDerives derives
            ]
        ]
    format Haskell.ValueDecl{name} = hcat
        [ format name, " = error \"TODO\"" ]

formatDerives :: [Name.UnQ] -> PP.Doc
formatDerives [] = ""
formatDerives ds = "deriving" <> PP.tupled (map format ds)

instance Format Name.UnQ where
    format = PP.textStrict . Name.renderUnQ

instance Format Haskell.DataVariant where
    format Haskell.DataVariant{dvCtorName, dvArgs} =
        hcat [ format dvCtorName, " ", format dvArgs ]

instance Format Haskell.DataArgs where
    format (Haskell.PosArgs types) =
        mconcat $ intersperse " " (map format types)

instance Format Haskell.Type where
    format (Haskell.PrimType ty)  = format ty
    format (Haskell.NamedType ty) = format ty

instance Format Name.GlobalQ where
    format Name.GlobalQ{local, globalNS=Name.NS parts} =
        mconcat
            [ mconcat $ intersperse "." (map PP.textStrict parts)
            , "."
            , PP.textStrict $ Name.renderLocalQ local
            ]

instance Format Haskell.Import where
    format Haskell.Import{importAs, parts} = hcat
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
