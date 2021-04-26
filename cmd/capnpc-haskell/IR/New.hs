{-# LANGUAGE DuplicateRecordFields #-}
module IR.New
    ( File(..)
    , Decl(..)
    , UnionVariant(..)
    , Brand
    , ExtraTypeInfo(..)
    ) where

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

data ExtraTypeInfo
    = StructTypeInfo
        { nWords :: !Word16
        , nPtrs  :: !Word16
        }

data UnionVariant = UnionVariant
    { variantName  :: Name.UnQ
    , tagValue     :: !Word16
    , fieldLocType :: C.FieldLocType Brand Name.CapnpQ
    }
