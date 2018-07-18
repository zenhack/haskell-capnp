module Data.Capnp where


newtype Get m parent child = Get { get :: parent -> m child }
newtype Has m parent = Has { has :: parent -> m Bool }
newtype Set m parent child = Set { set :: parent -> child -> m () }
