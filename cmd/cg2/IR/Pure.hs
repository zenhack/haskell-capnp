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
    }

data Decl
    = Data
        { typeName   :: Name.LocalQ
        , variants   :: [Variant]
        , isUnion    :: !Bool
        , cerialName :: Name.LocalQ
        -- ^ The name of the type our 'Cerial' should be. This will only be
        -- different from typeName if we're an anonymous union in a struct
        -- that also has other fields; in this case our Cerial should be
        -- the same as our parent struct.
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
