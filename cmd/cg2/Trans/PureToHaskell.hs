{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Trans.PureToHaskell where

import IR.Haskell

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

declToDecls :: P.Decl -> [Decl]
declToDecls P.DStruct{typeName, fields} =
    let name = Name.localToUnQ typeName in
    [ DcData Data
        { dataName = name
        , typeArgs = []
        , derives = [ {- TODO. should factor out some of the helpers from RawToHaskell. -} ]
        , dataNewtype = False
        , dataVariants =
            [ DataVariant
                { dvCtorName = name
                , dvArgs = ARec (map fieldToField fields)
                }
            ]
        }
    ]
declToDecls _ = error "TODO"

fieldToField :: P.Field -> (Name.UnQ, Type)
fieldToField P.Field{name, type_} = (name, typeToType type_)

typeToType :: C.Type Name.CapnpQ -> Type
typeToType = error "TODO"
