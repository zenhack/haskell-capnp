{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Trans.FlatToNew (cgrToFiles) where

import Data.Bifunctor (Bifunctor(..))

import qualified Capnp.Repr as R
import           Data.Maybe (isJust)
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

mapTypes :: (Bifunctor p, Functor f) => p (f Flat.Node) Flat.Node -> p (f Name.CapnpQ) Name.CapnpQ
mapTypes = C.bothMap (\Flat.Node{name} -> name)

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

        mkMethod methodId Flat.Method{name, paramType, resultType} =
            New.MethodDecl
                { interfaceName = local
                , interfaceId = nodeId
                , typeParams = map C.paramName typeParams
                , methodName = name
                , methodId
                , paramType = mapTypes paramType
                , resultType = mapTypes resultType
                }

        parsedStructNode fields hasUnion isGroup =
            New.ParsedInstanceDecl
                { typeName = local
                , typeParams = map C.paramName typeParams
                , parsedDef = New.ParsedStruct
                    { fields =
                        [ ( Name.getUnQ fieldName
                          , mapTypes fieldLocType
                          )
                        | Flat.Field{fieldName, fieldLocType} <- fields
                        ]
                    , hasUnion
                    , dataCtorName = dataCtorName isGroup
                    }
                }

        parsedUnionNode Flat.Union{variants} =
            New.ParsedInstanceDecl
                { typeName = local
                , typeParams = map C.paramName typeParams
                , parsedDef = New.ParsedUnion
                    { variants =
                        [ ( Name.getUnQ fieldName
                          , mapTypes fieldLocType
                          )
                        | Flat.Variant{field=Flat.Field{fieldName, fieldLocType}} <- variants
                        ]
                    }
                }

        dataCtorName isGroup
            | isGroup = Name.mkSub local ""
            | otherwise = local

        structParseInstance fields hasUnion isGroup =
            New.ParseInstanceDecl
                { typeName = local
                , typeParams = map C.paramName typeParams
                , parseInstance = New.StructParseInstance
                    { fields = [ Name.getUnQ fieldName | Flat.Field{fieldName} <- fields ]
                    , hasUnion
                    , dataCtorName = dataCtorName isGroup
                    }
                }


        structUnionNodes Nothing = []
        structUnionNodes (Just union@Flat.Union{tagOffset, variants}) =
            [ New.UnionDecl
                { name = local
                , typeParams = map C.paramName typeParams
                , tagLoc = C.DataLoc
                    { dataIdx = fromIntegral $ tagOffset `div` 4
                    , dataOff = fromIntegral $ (tagOffset `mod` 4) * 16
                    , dataDef = 0
                    }
                , variants = map variantToVariant variants
                }
            , parsedUnionNode union
            ]
    in
    case union_ of
        Flat.Other       -> []
        Flat.Constant _  -> []
        Flat.Enum enumerants ->
            [ mkType (R.Data R.Sz16) $ Just $ New.EnumTypeInfo enumerants ]
        Flat.Interface{methods} ->
            mkType (R.Ptr (Just R.Cap)) Nothing
            : zipWith mkMethod [0..] methods
        Flat.Struct{isGroup, fields, union, dataWordCount = nWords, pointerCount = nPtrs} ->
            mkType (R.Ptr (Just R.Struct)) (Just New.StructTypeInfo { nWords, nPtrs })
            : parsedStructNode fields (isJust union) isGroup
            : structParseInstance fields (isJust union) isGroup
            : (structUnionNodes union ++ map mkField fields)

fieldToDecl :: Name.LocalQ -> [C.TypeParamRef Flat.Node] -> Flat.Field -> New.Decl
fieldToDecl containerType typeParams Flat.Field{fieldName, fieldLocType} =
    New.FieldDecl
        { containerType
        , typeParams = map C.paramName typeParams
        , fieldName = Name.getUnQ fieldName
        , fieldLocType = mapTypes fieldLocType
        }

variantToVariant :: Flat.Variant -> New.UnionVariant
variantToVariant Flat.Variant{tagValue, field = Flat.Field{fieldName, fieldLocType}} =
    New.UnionVariant
        { variantName = Name.getUnQ fieldName
        , tagValue
        , fieldLocType = mapTypes fieldLocType
        }
