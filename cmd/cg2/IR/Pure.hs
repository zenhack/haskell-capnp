{-# LANGUAGE DuplicateRecordFields #-}
module IR.Pure where

import Data.Word

import qualified IR.Common as C
import qualified IR.Name   as Name

data File = File
    { fileId        :: !Word64
    , fileName      :: FilePath
    , fileImports   :: [Word64]
    , decls         :: [Decl]
    , reExportEnums :: [Name.LocalQ]
    -- ^ A list of enums that we should re-export from this module.
    , usesRpc       :: !Bool
    -- ^ Whether or not the module uses rpc features. If not, we skip
    -- the rpc related imports. This is mainly important to avoid a
    -- cyclic dependency with rpc.capnp.
    }

data Decl
    = Data
        { typeName   :: Name.LocalQ
        , variants   :: [Variant]
        , firstClass :: !Bool
        -- ^ Whether this is a "first class" type, i.e. it is a type in the
        -- capnproto sense, rather than an auxiliary type defined for a group
        -- or an anonymous union.
        --
        -- Note that this *can* be set for unions, if they subsume the whole
        -- struct, since in that case we collapse the two types in the
        -- high-level API.
        , isUnion    :: !Bool
        -- ^ Whether or not this is a union. This controls things like
        -- whether generated code needs to set a tag, and deal with unknown'
        -- variants.
        , cerialName :: Name.LocalQ
        -- ^ The name of the type our 'Cerial' should be. This will only be
        -- different from typeName if we're an anonymous union in a struct
        -- that also has other fields; in this case our Cerial should be
        -- the same as our parent struct.
        }
    | Constant
        { name  :: Name.LocalQ
        , value :: C.Value Name.CapnpQ
        }
    | Interface
        { name :: Name.LocalQ
        }

data Field = Field
    { name  :: Name.UnQ
    -- ^ The name of the field.

    , type_ :: C.Type Name.CapnpQ
    -- ^ The type of the field.
    }

data Variant = Variant
    { name :: Name.LocalQ
    , arg  :: Argument
    }

data Argument
    = None
    | Positional (C.Type Name.CapnpQ)
    | Record [Field]
