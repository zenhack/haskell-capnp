{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
module IR.Name where

import Data.List   (intersperse)
import Data.String (IsString)

import qualified Data.Text as T

newtype UnQ = UnQ T.Text
    deriving(Show, Read, Eq, Ord, IsString)

newtype NS = NS [T.Text]
    deriving(Show, Read, Eq, Ord)

data LocalQ = LocalQ
    { localUnQ :: UnQ
    , localNS  :: NS
    }
    deriving(Show, Read, Eq, Ord)

data GlobalQ = GlobalQ
    { local    :: LocalQ
    , globalNS :: NS
    }

emptyNS :: NS
emptyNS = NS []

mkLocal :: NS -> UnQ -> LocalQ
mkLocal localNS localUnQ = LocalQ{localNS, localUnQ}

mkSub :: LocalQ -> UnQ -> LocalQ
mkSub q unQ = mkLocal (localQToNS q) unQ

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
