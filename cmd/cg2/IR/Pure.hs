{-# LANGUAGE DuplicateRecordFields #-}
module IR.Pure where

import qualified IR.Name as Name

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
    { name     :: Name.UnQ
    -- ^ The name of the field.

    , cerialEq :: !Bool
    -- ^ Whether the serialized and unserialized forms of this type
    -- are the same. If not, there is a marshalling step, if so, the
    -- accessors work with the decerialized form directly.
    --
    -- For example, this is True for Enums, basic integers, etc. but
    -- False for structs, interfaces, etc.
    }

data Variant = Variant
    { name   :: Name.LocalQ
    , fields :: [Field]
    }
