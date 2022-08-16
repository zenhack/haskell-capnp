{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Module: Capnp.New.Constraints
-- Description: convenience shorthands for various constraints.
module Capnp.New.Constraints where

import qualified Capnp.New.Classes as C
import qualified Capnp.Repr as R
import qualified Capnp.Repr.Parsed as RP

-- | Constraints needed for @a@ to be a capnproto type parameter.
type TypeParam a =
  ( R.IsPtr a,
    C.Parse a (RP.Parsed a)
  )
