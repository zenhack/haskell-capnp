{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Trans.FlatToNew (cgrToFiles) where

import qualified Capnp.Repr as R
import qualified IR.Common  as C
import qualified IR.Flat    as Flat
import qualified IR.Name    as Name
import qualified IR.New     as New

cgrToFiles :: Flat.CodeGenReq -> [New.File]
cgrToFiles = map fileToFile . Flat.reqFiles

fileToFile :: Flat.File -> New.File
fileToFile Flat.File{fileId, fileName, nodes} =
    New.File
        { fileId
        , fileName
        , decls = concatMap nodeToDecls nodes
        }

nodeToDecls :: Flat.Node -> [New.Decl]
nodeToDecls Flat.Node{nodeId, name=Name.CapnpQ{local}, typeParams, union_} =
    let mkType repr extraTypeInfo =
            New.TypeDecl
                { name = local
                , nodeId
                , params = map C.paramName typeParams
                , repr
                , extraTypeInfo
                }
        mkField field =
            fieldToDecl local typeParams field

        structUnionNodes Nothing = []
        structUnionNodes (Just Flat.Union{tagOffset, variants}) =
            [ New.UnionDecl
                { name = local
                , typeParams = map C.paramName typeParams
                , tagLoc = C.DataLoc
                    { dataIdx = fromIntegral $ tagOffset `div` 4
                    , dataOff = fromIntegral $ tagOffset `mod` 4
                    , dataDef = 0
                    }
                , variants = map variantToVariant variants
                }
            ]
    in
    case union_ of
        Flat.Other       -> []
        Flat.Constant _  -> []
        Flat.Enum _      -> [ mkType (R.Data R.Sz16) Nothing ]
        Flat.Interface{} -> [ mkType (R.Ptr (Just R.Cap)) Nothing ]
        Flat.Struct{fields, union, dataWordCount = nWords, pointerCount = nPtrs} ->
            mkType (R.Ptr (Just R.Struct)) (Just New.StructTypeInfo { nWords, nPtrs })
            : (structUnionNodes union ++ map mkField fields)

fieldToDecl :: Name.LocalQ -> [C.TypeParamRef Flat.Node] -> Flat.Field -> New.Decl
fieldToDecl containerType typeParams Flat.Field{fieldName, fieldLocType} =
    New.FieldDecl
        { containerType
        , typeParams = map C.paramName typeParams
        , fieldName = Name.getUnQ fieldName
        , fieldLocType = C.bothMap (\Flat.Node{name} -> name) fieldLocType
        }

variantToVariant :: Flat.Variant -> New.UnionVariant
variantToVariant Flat.Variant{tagValue, field = Flat.Field{fieldName, fieldLocType}} =
    New.UnionVariant
        { variantName = Name.getUnQ fieldName
        , tagValue
        , fieldLocType = C.bothMap (\Flat.Node{name} -> name) fieldLocType
        }
