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

emptyNS :: NS
emptyNS = NS []

mkLocal :: NS -> UnQ -> LocalQ
mkLocal localNS localUnQ = LocalQ{localNS, localUnQ}

localQToNS :: LocalQ -> NS
localQToNS LocalQ{localUnQ=UnQ part, localNS=NS parts} = NS (part:parts)

renderUnQ :: UnQ -> T.Text
renderUnQ (UnQ name) = name

renderLocalQ :: LocalQ -> T.Text
renderLocalQ LocalQ{localUnQ, localNS}
    | localNS == emptyNS = renderUnQ localUnQ
    | otherwise = renderLocalNS localNS <> "'" <> renderUnQ localUnQ

renderLocalNS :: NS -> T.Text
renderLocalNS (NS parts) = mconcat $ intersperse "'" $ reverse parts
