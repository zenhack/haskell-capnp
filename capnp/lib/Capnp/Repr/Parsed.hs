{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Capnp.Repr.Parsed (Parsed) where

import qualified Capnp.Basics as B
import qualified Capnp.Classes as C
import Capnp.Repr (List, PtrRepr (..), Repr (..), ReprFor)
import Capnp.Rpc.Common (Client)
import qualified Data.ByteString as BS
import Data.Kind (Type)
import qualified Data.Text as T

-- | @'Parsed' a@ is the high-level/ADT representation of the capnproto
-- type @a@. For struct types this is equivalent to @'C.Parsed' a@, but
-- we special case other types, such that e.g.
-- @'Parsed' 'B.Data'@ = 'BS.ByteString'.
type Parsed a = ParsedByRepr (ReprFor a) a

-- Helper for 'Parsed'
type family ParsedByRepr (r :: Repr) (a :: Type) where
  ParsedByRepr ('Data _) a = a
  ParsedByRepr ('Ptr ('Just 'Cap)) a = Client a
  ParsedByRepr _ B.Data = BS.ByteString
  ParsedByRepr _ B.Text = T.Text
  ParsedByRepr _ (List a) = [Parsed a]
  ParsedByRepr _ (Maybe B.AnyPointer) = Maybe (C.Parsed B.AnyPointer)
  ParsedByRepr _ a = C.Parsed a
