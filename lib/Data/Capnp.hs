module Data.Capnp where

-- | A 'Get' is a wrapper around a @get_*@ function for a field. This is
-- intended to be used with the OverloadedLabels extension, like
-- @'get' #myField struct@. The newtype wrapper and 'get' function allow
-- us to disambiguate between different uses of @#myField@.
newtype Get a = Get { get :: a }

-- | Like 'Get', but wraps a @has_*@ function instead of a @get_*@ function.
newtype Has a = Has { has :: a }

-- | Like 'Get', but wraps a @set_*@ function instead of a @get_*@ function.
newtype Set a = Set { set :: a }

-- | Like 'Get', but wraps a @new_*@ function instead of a @get_*@ function.
newtype New a = New { new :: a }
