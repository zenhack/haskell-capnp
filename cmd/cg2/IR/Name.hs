{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
module IR.Name where

import Data.Word

import Data.List   (intersperse)
import Data.String (IsString(fromString))

import qualified Data.Text as T

class HasUnQ a where
    getUnQ :: a -> UnQ
class MkSub a where
    mkSub :: a -> UnQ -> a

instance HasUnQ UnQ where
    getUnQ = id
instance HasUnQ LocalQ where
    getUnQ = localUnQ
instance HasUnQ CapnpQ where
    getUnQ CapnpQ{local} = getUnQ local
instance HasUnQ GlobalQ where
    getUnQ GlobalQ{local} = getUnQ local

newtype UnQ = UnQ T.Text
    deriving(Show, Read, Eq, Ord, IsString)

newtype NS = NS [T.Text]
    deriving(Show, Read, Eq, Ord)

data LocalQ = LocalQ
    { localUnQ :: UnQ
    , localNS  :: NS
    }
    deriving(Show, Read, Eq, Ord)

instance IsString LocalQ where
    fromString s = LocalQ
        { localUnQ = fromString s
        , localNS = emptyNS
        }

-- | A fully qualified name for something defined in a capnproto schema.
-- this includes a local name within a file, and the file's capnp id.
data CapnpQ = CapnpQ
    { local  :: LocalQ
    , fileId :: !Word64
    }
    deriving(Show, Read, Eq, Ord)

data GlobalQ = GlobalQ
    { local    :: LocalQ
    , globalNS :: NS
    }
    deriving(Show, Read, Eq, Ord)

emptyNS :: NS
emptyNS = NS []

mkLocal :: NS -> UnQ -> LocalQ
mkLocal localNS localUnQ = LocalQ{localNS, localUnQ}

instance MkSub LocalQ where
    mkSub q = mkLocal (localQToNS q)

instance MkSub GlobalQ where
    mkSub q@GlobalQ{local} unQ = q { local = mkSub local unQ }

instance MkSub CapnpQ where
    mkSub q@CapnpQ{local} unQ = q { local = mkSub local unQ }

localQToNS :: LocalQ -> NS
localQToNS LocalQ{localUnQ=UnQ part, localNS=NS parts} = NS (part:parts)

localToUnQ :: LocalQ -> UnQ
localToUnQ LocalQ{localUnQ, localNS}
    | localNS == emptyNS = localUnQ
    | otherwise = UnQ (renderLocalNS localNS <> "'" <> renderUnQ localUnQ)

renderUnQ :: UnQ -> T.Text
renderUnQ (UnQ name) = name

renderLocalQ :: LocalQ -> T.Text
renderLocalQ = renderUnQ . localToUnQ

renderLocalNS :: NS -> T.Text
renderLocalNS (NS parts) = mconcat $ intersperse "'" $ reverse parts
