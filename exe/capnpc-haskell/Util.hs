-- | Misc. helpers.
module Util (Id(..), mintercalate) where

import Data.Word

import Data.List   (intersperse)
import Data.Monoid (Monoid, mconcat)

-- | Generalization of 'Data.List.intercalate', analogous to concat/mconcat
mintercalate :: Monoid w => w -> [w] -> w
mintercalate sep = mconcat . intersperse sep

-- | Type alias for the Ids used in the capnproto schema; this is defined
-- in the source schema, but the schema compiler doesn't include information
-- on type aliases in the message it sends to the plugin, so we have no way
-- of automating the alias.
type Id = Word64
