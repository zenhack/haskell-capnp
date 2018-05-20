-- | Misc. helpers.
module Util (mintercalate) where

import Data.List   (intersperse)
import Data.Monoid (Monoid, mconcat)

-- | Generalization of 'Data.List.intercalate', analogous to concat/mconcat
mintercalate :: Monoid w => w -> [w] -> w
mintercalate sep = mconcat . intersperse sep
