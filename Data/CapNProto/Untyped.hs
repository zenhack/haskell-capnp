{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, FunctionalDependencies #-}
{-|
Module: Data.CapNProto.Untyped
Description: Utilities for manipulating capnproto messages with no schema.

The types and functions in this module know about things like structs and
lists, but are not schema aware.
-}
module Data.CapNProto.Untyped where
