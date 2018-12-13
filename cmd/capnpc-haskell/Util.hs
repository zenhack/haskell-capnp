-- | Misc. helpers.
module Util (Id, mintercalate, splitOn) where

import Data.Word

import Data.List (intersperse)

-- | Generalization of 'Data.List.intercalate', analogous to concat/mconcat
mintercalate :: Monoid w => w -> [w] -> w
mintercalate sep = mconcat . intersperse sep

-- | Type alias for the Ids used in the capnproto schema; this is defined
-- in the source schema, but the schema compiler doesn't include information
-- on type aliases in the message it sends to the plugin, so we have no way
-- of automating the alias.
type Id = Word64

-- | @'splitOn' sep str@ splits the string @str@ on boundaries
-- equal to @sep@. This is a misc. helper function that really should be
-- in the stdlib, but it isn't.
splitOn :: Char -> String -> [String]
splitOn target = go [] where
    go [] [] = []
    go acc [] = [reverse acc]
    go acc (c:cs)
        | c == target = reverse acc : go [] cs
        | otherwise = go (c:acc) cs
