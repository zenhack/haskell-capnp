{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Module: Capnp.Constraints
-- Description: convenience shorthands for various constraints.
module Capnp.Constraints where

import qualified Capnp.Classes as C
import qualified Capnp.Repr as R
import qualified Capnp.Repr.Parsed as RP

-- | Constraints needed for @a@ to be a capnproto type parameter.
type TypeParam a =
  ( R.IsPtr a,
    C.Parse a (RP.Parsed a)
  )
