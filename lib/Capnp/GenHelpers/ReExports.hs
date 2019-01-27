-- |
-- Module: Capnp.GenHelpers.ReExports
-- Description: Re-exported modules from common libraries.
--
-- This module heirarchy exists so that generated code doesn't have to
-- import anything outside of base and the capnp package; the generated
-- code uses modules from several common libraries (bytestring, vector,
-- text...), but we would prefer not to burden developers packaging schema
-- with having to list each of these in their build-depends, so instead
-- we re-export the modules from here.
module Capnp.GenHelpers.ReExports() where
