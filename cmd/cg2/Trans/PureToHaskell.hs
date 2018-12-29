{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Trans.PureToHaskell where

import Data.Word

import Text.Printf (printf)

import qualified Data.Text as T

import IR.Haskell
import Trans.ToHaskellCommon

import qualified IR.Common as C
import qualified IR.Name   as Name
import qualified IR.Pure   as P

-- | Whether the serialized and unserialized forms of this type
-- are the same. If not, there is a marshalling step, if so, the
-- accessors work with the decerialized form directly.
--
-- For example, this is True for Enums, basic integers, etc. but
-- False for structs, interfaces, etc.
cerialEq :: C.Type r -> Bool
cerialEq C.VoidType          = True
cerialEq (C.WordType _)      = True
cerialEq (C.CompositeType _) = False
cerialEq (C.PtrType _)       = False

fileToModule :: P.File -> Module
fileToModule P.File{fileName, fileId, decls} = Module
    { modName = ["Capnp", "Gen"] ++ makeModName fileName ++ ["Pure"]
    , modLangPragmas =
        [ "DuplicateRecordFields"
        ]
    , modImports =
        [ Import { importAs = "V", parts = ["Data", "Vector"] }
        , Import { importAs = "T", parts = ["Data", "Text"] }
        , Import { importAs = "BS", parts = ["Data", "ByteString"] }
        , Import { importAs = "UntypedPure", parts = ["Capnp", "Untyped", "Pure"] }
        ]
    , modDecls = concatMap (declToDecls fileId) decls
    }

declToDecls :: Word64 -> P.Decl -> [Decl]
declToDecls thisMod P.DStruct{typeName, fields} =
    let name = Name.localToUnQ typeName in
    [ DcData Data
        { dataName = name
        , typeArgs = []
        , derives = [ {- TODO. should factor out some of the helpers from RawToHaskell. -} ]
        , dataNewtype = False
        , dataVariants =
            [ DataVariant
                { dvCtorName = name
                , dvArgs = ARec (map (fieldToField thisMod) fields)
                }
            ]
        }
    , instance_ [] ["Classes"] "Decerialize" [TLName typeName]
        [ iValue "decerialize" [PVar "raw"] $
            if null fields then
                EApp (eStd_ "pure") [ELName typeName]
            else
                EFApp
                    (ELName typeName)
                    [
                        let getter =
                                EApp
                                    (egName
                                        (rawModule thisMod)
                                        (Name.mkSub typeName name))
                                    [ELName "raw"]
                        in
                        if cerialEq type_ then
                            getter
                        else
                            EBind getter (egName ["Classes"] "decerialize")
                    | P.Field{name, type_} <- fields
                    ]
        ]
    ]
declToDecls _thisMod _ = [] -- TODO

fieldToField :: Word64 -> P.Field -> (Name.UnQ, Type)
fieldToField thisMod P.Field{name, type_} = (name, typeToType thisMod type_)

typeToType :: Word64 -> C.Type Name.CapnpQ -> Type
typeToType _thisMod C.VoidType                        = TUnit
typeToType _thisMod (C.WordType (C.PrimWord ty))      = TPrim ty
typeToType thisMod (C.WordType (C.EnumType n))        = nameToType thisMod n
typeToType thisMod (C.CompositeType (C.StructType n)) = nameToType thisMod n
typeToType thisMod (C.PtrType (C.PtrComposite (C.StructType n))) =
    nameToType thisMod n
typeToType thisMod (C.PtrType (C.ListOf ty)) =
    TApp (tgName ["V"] "Vector") [typeToType thisMod ty]
typeToType _thisMod (C.PtrType (C.PrimPtr C.PrimText)) =
    tgName ["T"] "Text"
typeToType _thisMod (C.PtrType (C.PrimPtr C.PrimData)) =
    tgName ["BS"] "ByteString"
typeToType _thisMod (C.PtrType (C.PrimPtr (C.PrimAnyPtr _))) =
    -- TODO: distinguish different pointer types.
    TApp (tStd_ "Maybe") [tgName ["UntypedPure"] "Ptr"]
typeToType thisMod (C.PtrType (C.PtrInterface n)) =
    nameToType thisMod n

nameToType :: Word64 -> Name.CapnpQ -> Type
nameToType thisMod Name.CapnpQ{local, fileId}
    | thisMod == fileId = TLName local
    | otherwise = tgName (pureModule fileId) local

rawModule :: Word64 -> [T.Text]
rawModule modId =
    ["Capnp", "Gen", "ById", T.pack $ printf "X%x" modId]

pureModule :: Word64 -> [T.Text]
pureModule modId =
    rawModule modId ++ ["Pure"]
