{-# LANGUAGE DuplicateRecordFields #-}
module IR.New where

import qualified Capnp.Repr as R
import           Data.Word
import qualified IR.Common  as C
import qualified IR.Name    as Name

type Brand = C.ListBrand Name.CapnpQ

data File
    = File
        { fileId   :: !Word64
        , decls    :: [Decl]
        , fileName :: FilePath
        }

data Decl
    = TypeDecl
        { name          :: Name.LocalQ
        , nodeId        :: !Word64
        , params        :: [Name.UnQ]
        , repr          :: R.Repr
        , extraTypeInfo :: Maybe ExtraTypeInfo
        }
    | FieldDecl
        { containerType :: Name.LocalQ
        , typeParams    :: [Name.UnQ]
        , fieldName     :: Name.UnQ
        , fieldLocType  :: C.FieldLocType Brand Name.CapnpQ
        }
    | UnionDecl
        { name       :: Name.LocalQ
        , typeParams :: [Name.UnQ]
        , tagLoc     :: C.DataLoc
        , variants   :: [UnionVariant]
        }
    | MethodDecl
        { interfaceName :: Name.LocalQ
        , typeParams    :: [Name.UnQ]
        , interfaceId   :: !Word64
        , methodName    :: Name.UnQ
        , methodId      :: !Word16
        , paramType     :: C.CompositeType Brand Name.CapnpQ
        , resultType    :: C.CompositeType Brand Name.CapnpQ
        }
    | ParsedInstanceDecl
        { typeName        :: Name.LocalQ
        , typeParams      :: [Name.UnQ]
        , parsedInstances :: ParsedInstances
        }

-- | Data needed for declaring a Parsed instance, and instances
-- of related classes.
data ParsedInstances
    = ParsedStruct
        { fields       :: [(Name.UnQ, C.FieldLocType Brand Name.CapnpQ)]
        , hasUnion     :: !Bool
        , dataCtorName :: Name.LocalQ
        }
    | ParsedUnion
        { variants :: [(Name.UnQ, C.FieldLocType Brand Name.CapnpQ)]
        }

data ExtraTypeInfo
    = StructTypeInfo
        { nWords :: !Word16
        , nPtrs  :: !Word16
        }
    | EnumTypeInfo [Name.UnQ]
    | InterfaceTypeInfo

data UnionVariant = UnionVariant
    { variantName  :: Name.UnQ
    , tagValue     :: !Word16
    , fieldLocType :: C.FieldLocType Brand Name.CapnpQ
    }
