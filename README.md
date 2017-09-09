[![Travis CI][travis-img]][travis-ci]

Beginnings of a haskell [capnproto][1] library.

# Status

We have partial support for reading and building untyped messages (i.e.
working at the level of structs/lists/pointers, with no schema).
Currently we're bootstrapping readers for the schema.capnp schema, so we
can start doing codegen.

# License

MIT

[1]: https://capnproto.org/

[travis-ci]: https://travis-ci.org/zenhack/haskell-capnp
[travis-img]: https://travis-ci.org/zenhack/haskell-capnp.svg?branch=master
