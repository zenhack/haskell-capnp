module Data.Capnp where

-- | A 'Get' is a wrapper around a @get_*@ function for a field. This is
-- intended to be used with the OverloadedLabels extension, like
-- @'get' #myField struct@. The newtype wrapper and 'get' function allow
-- us to disambiguate between different uses of @#myField@.
newtype Get m parent child = Get { get :: parent -> m child }

-- | Like 'Get', but wraps a @has_*@ function instead of a @get_*@ function.
newtype Has m parent = Has { has :: parent -> m Bool }

-- | Like 'Get', but wraps a @set_*@ function instead of a @get_*@ function.
newtype Set m parent child = Set { set :: parent -> child -> m () }
