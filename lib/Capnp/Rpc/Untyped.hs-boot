module Capnp.Rpc.Untyped where

data Client
data Pipeline

nullClient :: Client

instance Eq Client
instance Show Client
