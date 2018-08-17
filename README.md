A Haskell [capnproto][1] library.

[![build status][ci-img]][ci]

# Status

Serialization (read & write) support is mostly finished, and already
usable, with some limitations:

* Generated schema currently ignore type parameters (#29).
* Schema which define custom default values for fields of pointer type
  are rejected (#28).
* We currently do not correctly handle decoding lists of structs from
  non-composite lists (#27). This means that, contrary to the [protocol
  evolution rules][2], it is not safe to change a field from type
  List(T) (where T is any non-struct type) to a list of a struct type.

The API is considered unstable. It will likely see changes both for the
sake of polish, consistency, etc. and to accommodate more features as we
add them (RPC in particular will probably require changing some
interfaces).

# License

MIT

[1]: https://capnproto.org/
[2]: https://capnproto.org/language.html#evolving-your-protocol

[ci-img]: https://gitlab.com/isd/haskell-capnp/badges/master/build.svg
[ci]: https://gitlab.com/isd/haskell-capnp/pipelines
