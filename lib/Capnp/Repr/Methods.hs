{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
-- | Module: Capnp.Repr.Methods
-- Description: Support for working with methods
module Capnp.Repr.Methods
    ( Method(..)
    , HasMethod
    ) where

import qualified Capnp.Repr           as R
import           Data.Word
import           GHC.OverloadedLabels (IsLabel)

-- | Represents a method on the interface type @c@ with parameter
-- type @p@ and return type @r@.
data Method c p r = Method
    { interfaceId :: !Word64
    , methodId    :: !Word16
    }

-- | An instance @'HasMethod' name c p r@ indicates that the interface
-- type @c@ has a method named @name@ with parameter type @p@ and
-- return type @r@. The generated code includes instances of this
-- for each method in the schema.
class
    ( R.ReprFor c ~ 'R.Ptr ('Just 'R.Cap)
    , IsLabel name (Method c p r)
    ) => HasMethod name c p r | name c -> p r
