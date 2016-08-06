module PrimTypes where

import Data.Word

type List a = [a]

data AnyPointer = AnyPointer Word64
