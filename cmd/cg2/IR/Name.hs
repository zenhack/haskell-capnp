{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
module IR.Name where

import Data.Word

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

data GlobalCapnp = GlobalCapnp
    { capLocal  :: LocalQ
    , capFileId :: !Word64
    }
    deriving(Show, Read, Eq, Ord)

isEmptyNS :: NS -> Bool
isEmptyNS (NS [])    = True
isEmptyNS (NS (_:_)) = False

renderUnQ :: UnQ -> T.Text
renderUnQ (UnQ name) = name

renderLocalQ :: LocalQ -> T.Text
renderLocalQ LocalQ{localUnQ, localNS}
    | isEmptyNS localNS = renderUnQ localUnQ
    | otherwise = renderLocalNS localNS <> "." <> renderUnQ localUnQ

renderLocalNS :: NS -> T.Text
renderLocalNS (NS parts) = mconcat (intersperse "'" parts)
