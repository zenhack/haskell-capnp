{-# LANGUAGE DuplicateRecordFields #-}
module IR.Pure where

import qualified IR.Common as C
import qualified IR.Name   as Name

data Decl
    = DUnion
        { typeName :: Name.LocalQ
        , variants :: [Variant]
        }
    | DStruct
        { typeName :: Name.LocalQ
        , fields   :: [Field]
        }

data Field = Field
    { name  :: Name.UnQ
    -- ^ The name of the field.

    , type_ :: C.Type Name.CapnpQ
    -- ^ The type of the field.
    }

data Variant = Variant
    { name   :: Name.LocalQ
    , fields :: [Field]
    }
