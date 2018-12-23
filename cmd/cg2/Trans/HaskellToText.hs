{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Trans.HaskellToText (moduleToText) where

import Data.List                    (intersperse)
import Data.String                  (fromString)
import Text.PrettyPrint.Leijen.Text (hcat, vcat)

import qualified Text.PrettyPrint.Leijen.Text as PP

import qualified IR.Common  as C
import qualified IR.Haskell as H
import qualified IR.Name    as Name

moduleToText :: H.Module -> PP.Doc
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

instance Format H.Module where
    format H.Module{modName, modDecls, modImports} = vcat
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
            [ H.Import { importAs = "Std_", parts = ["Prelude"] }
            , H.Import { importAs = "Std_", parts = ["Data", "Word"] }
            , H.Import { importAs = "Std_", parts = ["Data", "Int"] }
            ]
        , vcat $ map format modDecls
        ]

instance Format H.Decl where
    format H.DcData{dataName, typeArgs, dataVariants, dataNewtype, derives} = vcat
        [ hcat
            [ if dataNewtype
                then "newtype "
                else "data "
            , format dataName
            , " "
            , mconcat $ intersperse " " $ map PP.textStrict typeArgs
            ]
        , indent $ vcat
            [ case dataVariants of
                (d:ds) -> vcat $ ("= " <> format d) : map (("| " <>) . format) ds
                []     -> ""
            , formatDerives derives
            ]
        ]
    format H.DcValue{typ, def=def@H.DfValue{name}} = vcat
        [ hcat [ format name, " :: ", format typ ]
        , format def
        ]
    format H.DcInstance{ctx, typ, defs} = vcat
        [ hcat
            [ "instance "
            , format (H.TCtx ctx typ)
            , case defs of
                []  -> ""
                _:_ -> " where"
            ]
        , indent $ vcat $
            map format defs
        ]

instance Format H.ValueDef where
    format H.DfValue{name, value, params} = hcat
        [ format name
        , " "
        , hcat $ intersperse " " $ map format params
        , " = "
        , format value
        ]

instance Format H.Exp where
    format (H.EApp e es) = hcat
        [ "("
        , hcat $ intersperse " " $ map format (e:es)
        , ")"
        ]
    format (H.EFApp e []) = format e
    format (H.EFApp e es) = hcat
        [ "("
        , format e
        , " <$> "
        , hcat $ intersperse " <*> " $ map format es
        , ")"
        ]
    format (H.EGName e) = format e
    format (H.ELName e) = format e
    format (H.EInt n) = fromString (show n)
    format (H.EDo ds ex) = vcat
        [ "(do"
        , indent $ vcat (map format ds ++ [format ex, ")"])
        ]
    format (H.ETup es) = PP.tupled (map format es)

instance Format H.Do where
    format (H.DoBind var ex) = format var <> " <- " <> format ex

instance Format H.Pattern where
    format (H.PVar v) = PP.textStrict v
    format (H.PLCtor c ps) = hcat
        [ "("
        , mconcat $ intersperse " " (format c : map format ps)
        , ")"
        ]
    format (H.PInt n) = fromString (show n)

formatDerives :: [Name.UnQ] -> PP.Doc
formatDerives [] = ""
formatDerives ds = "deriving" <> PP.tupled (map format ds)

instance Format H.DataVariant where
    format H.DataVariant{dvCtorName, dvArgs} =
        hcat [ format dvCtorName, " ", format dvArgs ]

instance Format H.DataArgs where
    format (H.APos types) =
        mconcat $ intersperse " " (map format types)

instance Format H.Type where
    format (H.TGName ty) = format ty
    format (H.TLName ty) = format ty
    format (H.TVar txt)  = PP.textStrict txt
    format (H.TApp f xs) =
        "(" <> (mconcat $ intersperse " " $ map format (f:xs)) <> ")"
    format (H.TFn types) =
        mconcat $ intersperse " -> " $ map format types
    format (H.TCtx[] ty) = format ty
    format (H.TCtx constraints ty) =
        PP.tupled (map format constraints) <> " => " <> format ty
    format (H.TPrim ty) = format ty
    format H.TUnit = "()"

instance Format Name.GlobalQ where
    format Name.GlobalQ{local, globalNS=Name.NS parts} =
        mconcat
            [ mconcat $ intersperse "." (map PP.textStrict parts)
            , "."
            , format local
            ]

instance Format Name.LocalQ where
    format = PP.textStrict . Name.renderLocalQ

instance Format Name.UnQ where
    format = PP.textStrict . Name.renderUnQ


instance Format H.Import where
    format H.Import{importAs, parts} = hcat
        [ "import qualified "
        , mconcat $ intersperse "." (map format parts)
        , " as "
        , format importAs
        ]

instance Format C.PrimWord where
    format C.PrimBool = "Std_.Bool"
    format (C.PrimInt (C.IntType sign sz)) =
        let szDoc = fromString $ show $ C.sizeBits sz
            typePrefix = case sign of
                C.Signed   -> "Int"
                C.Unsigned -> "Word"
        in
        "Std_." <> typePrefix <> szDoc
    format C.PrimFloat32 = "Std_.Float"
    format C.PrimFloat64 = "Std_.Double"

-- | Indent the argument by four spaces.
indent :: PP.Doc -> PP.Doc
indent = PP.indent 4
