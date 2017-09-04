Beginnings of a haskell [capnproto][1] library.

# Status

We have partial support for reading and building untyped messages (i.e.
working at the level of structs/lists/pointers, with no schema).
Currently we're bootstrapping readers for the schema.capnp schema, so we
can start doing codegen.

# License

MIT

[1]: https://capnproto.org/
