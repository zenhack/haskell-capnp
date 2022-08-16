{-# LANGUAGE DuplicateRecordFields #-}

module IR.Flat
  ( File (..),
    CodeGenReq (..),
    Node (..),
    Node' (..),
    Field (..),
    Method (..),
    Variant (..),
    Union (..),
  )
where

import Data.Word
import qualified IR.Common as Common
import qualified IR.Name as Name

type Brand = Common.ListBrand Node

data CodeGenReq = CodeGenReq
  { allNodes :: [Node],
    reqFiles :: [File]
  }
  deriving (Show, Eq)

data File = File
  { nodes :: [Node],
    fileId :: !Word64,
    fileName :: FilePath
  }
  deriving (Show, Eq)

data Node = Node
  { name :: Name.CapnpQ,
    nodeId :: !Word64,
    union_ :: Node',
    typeParams :: [Common.TypeParamRef Node]
  }
  deriving (Show, Eq)

data Node'
  = Enum [Name.UnQ]
  | Struct
      { -- | The struct's fields, excluding an anonymous union, if any.
        fields :: [Field],
        isGroup :: !Bool,
        dataWordCount :: !Word16,
        pointerCount :: !Word16,
        -- | The struct's anonymous union, if any.
        union :: Maybe Union
      }
  | Interface
      { methods :: [Method],
        supers :: [Common.InterfaceType Brand Node]
      }
  | Constant
      { value :: Common.Value Brand Node
      }
  | Other
  deriving (Show, Eq)

data Method = Method
  { name :: Name.UnQ,
    paramType :: Common.CompositeType Brand Node,
    resultType :: Common.CompositeType Brand Node
  }
  deriving (Show, Eq)

data Union = Union
  { tagOffset :: !Word32,
    variants :: [Variant]
  }
  deriving (Show, Eq)

data Field = Field
  { fieldName :: Name.CapnpQ,
    fieldLocType :: Common.FieldLocType Brand Node
  }
  deriving (Show, Eq)

data Variant = Variant
  { tagValue :: !Word16,
    -- | The field's name is really the name of the variant.
    field :: Field
  }
  deriving (Show, Eq)
