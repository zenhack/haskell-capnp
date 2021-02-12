{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Trans.NewToHaskell
    ( fileToModules
    ) where

import qualified Capnp.Repr            as R
import           Data.String           (IsString(fromString))
import           Data.Word
import qualified IR.Common             as C
import qualified IR.Haskell            as Hs
import qualified IR.Name               as Name
import qualified IR.New                as New
import           Trans.ToHaskellCommon

imports :: [Hs.Import]
imports =
    [ Hs.ImportAs { importAs = "R", parts = ["Capnp", "Repr"] }
    , Hs.ImportAs { importAs = "F", parts = ["Capnp", "Fields"] }
    , Hs.ImportAs { importAs = "OL", parts = ["GHC", "OverloadedLabels"] }
    ]

fileToModules :: New.File -> [Hs.Module]
fileToModules file@New.File{fileName} =
    [ Hs.Module
        { modName = ["Capnp", "Gen"] ++ makeModName fileName ++ ["New"]
        , modLangPragmas =
            [ "TypeFamilies"
            , "DataKinds"
            ]
        , modExports = Nothing
        , modImports = imports
        , modDecls = fileToDecls file
        }
    ]

fileToDecls :: New.File -> [Hs.Decl]
fileToDecls New.File{fileId, decls} =
    concatMap (declToDecls fileId) decls


declToDecls :: Word64 -> New.Decl -> [Hs.Decl]
declToDecls thisMod decl =
    case decl of
        New.TypeDecl {name, params, repr} ->
            let dataName = Name.localToUnQ name
                typeArgs = toTVars params
            in
            [ Hs.DcData Hs.Data
                { dataName
                , typeArgs
                , dataVariants = []
                , derives = []
                , dataNewtype = False
                }
            , Hs.DcTypeInstance
                (Hs.TApp
                    (tgName ["R"] "ReprFor")
                    [ case typeArgs of
                        [] -> tuName dataName
                        _  -> Hs.TApp (tuName dataName) typeArgs
                    ]
                )
                (toType repr)
            ]
        New.FieldDecl{containerType, typeParams, fieldName, fieldType} ->
            let ctx = []
                labelType = Hs.TString fieldName
                parentType = Hs.TApp (Hs.TLName containerType) (toTVars typeParams)
                childType = typeToType thisMod fieldType
            in
            [ Hs.DcInstance
                { ctx
                , typ = Hs.TApp (tgName ["OL"] "IsLabel")
                    [ labelType
                    , Hs.TApp (tgName ["F"] "Field") [parentType, childType]
                    ]
                , defs = []
                }
            , Hs.DcInstance
                { ctx
                , typ = Hs.TApp (tgName ["F"] "HasField") [labelType, parentType, childType]
                , defs = []
                }
            ]

tCapnp :: Word64 -> Name.CapnpQ -> Hs.Type
tCapnp thisMod Name.CapnpQ{local, fileId}
    | thisMod == fileId = Hs.TLName local
    | otherwise = tgName (map Name.renderUnQ $ idToModule fileId ++ ["New"]) local

typeToType :: Word64 -> C.Type New.Brand Name.CapnpQ -> Hs.Type
typeToType thisMod = \case
    C.VoidType                 -> Hs.TUnit
    C.WordType (C.EnumType nm) -> tCapnp thisMod nm
    _                          -> error "TODO"


class ToType a where
    toType :: a -> Hs.Type

instance ToType R.Repr where
    toType (R.Ptr p)  = rApp "Ptr" [toType p]
    toType (R.Data d) = rApp "Data" [toType d]

instance ToType a => ToType (Maybe a) where
    toType Nothing  = tStd_ "Nothing"
    toType (Just a) = Hs.TApp (tStd_ "Just") [toType a]

instance ToType R.PtrRepr where
    toType R.Cap      = tReprName "Cap"
    toType (R.List r) = rApp "List" [toType r]
    toType R.Struct   = tReprName "Struct"

instance ToType R.ListRepr where
    toType (R.ListNormal nl) = rApp "ListNormal" [toType nl]
    toType R.ListComposite   = tReprName "ListComposite"

instance ToType R.NormalListRepr where
    toType (R.ListData r) = rApp "ListData" [toType r]
    toType R.ListPtr      = tReprName "ListPtr"

instance ToType R.DataSz where
    toType = tReprName . fromString . show


rApp :: Name.LocalQ -> [Hs.Type] -> Hs.Type
rApp n = Hs.TApp (tReprName n)

tReprName :: Name.LocalQ -> Hs.Type
tReprName = tgName ["R"]
