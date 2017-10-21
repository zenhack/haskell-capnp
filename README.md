[![Travis CI][travis-img]][travis-ci]
[Hydra build status][hydra]

Beginnings of a haskell [capnproto][1] library.

# Status

We have partial support for reading and building untyped messages (i.e.
working at the level of structs/lists/pointers, with no schema).
Manually stubbed out readers for the datatypes in schema.capnp are
partially written, and a A code generator is in progress that will be
able to replace them.

# License

MIT

[1]: https://capnproto.org/

[travis-ci]: https://travis-ci.org/zenhack/haskell-capnp
[travis-img]: https://travis-ci.org/zenhack/haskell-capnp.svg?branch=master
[hydra]: https://hydra.angeldsis.com/jobset/haskell-capnp/zenhack-master
