This directory contains a few helper scripts for development.

* `regen.sh` rebuilds the schema compiler plugin, and uses it to
  re-generate modules for the core capnproto schema.
* `format.sh` runs `stylish-haskell` on the source tree (except for
  generated code).
* `hlint.sh` runs `hlint` on the source tree (except for generated
  code).
* `gen-basic-instances.hs` generates the module
  `Internal.Gen.Instances`, which contains a lot of boilerplate.
