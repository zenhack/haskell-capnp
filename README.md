Beginnings of a haskell [capnproto][1] library.

[![build status][ci-img]][ci]

# Status

We have partial support for reading and building untyped messages (i.e.
working at the level of structs/lists/pointers, with no schema).
Manually stubbed out readers for the datatypes in schema.capnp are
partially written, and a A code generator is in progress that will be
able to replace them.

# License

MIT

[1]: https://capnproto.org/

[ci-img]: https://gitlab.com/isd/haskell-capnp/badges/master/build.svg
[ci]: https://gitlab.com/isd/haskell-capnp/pipelines
