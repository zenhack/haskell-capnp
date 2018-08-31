[![build status][ci-img]][ci]
[![hackage][hackage-img]][hackage]

A Haskell library for the [Cap'N Proto][1] Cerialization protocol.

Serialization (read & write) support is mostly finished, and already
usable, with some limitations:

* Generated schema currently ignore type parameters ([#29][issue29]).
* Schema which define custom default values for fields of pointer type
  are rejected ([#28][issue28]).
* We currently do not correctly handle decoding lists of structs from
  non-composite lists ([#27][issue27]). This means that, contrary to the
  [protocol evolution rules][2], it is not safe to change a field from
  type List(T) (where T is any non-struct type) to a list of a struct
  type.

There is a module `Data.Capnp.Tutorial` which contains an introduction
to the library; users are *strongly* encouraged to read this first, as
the reference documentation can be bewildering without that context.

The API is considered unstable. It will likely see changes, for the
sake of polish, consistency, etc. as well as to improve performance and
accommodate more features as we add them (RPC in particular will
probably require changing some interfaces).

[1]: https://capnproto.org/
[2]: https://capnproto.org/language.html#evolving-your-protocol

[issue27]: https://github.com/zenhack/haskell-capnp/issues/27
[issue28]: https://github.com/zenhack/haskell-capnp/issues/28
[issue29]: https://github.com/zenhack/haskell-capnp/issues/29

[ci-img]: https://gitlab.com/isd/haskell-capnp/badges/master/build.svg
[ci]: https://gitlab.com/isd/haskell-capnp/pipelines

[hackage-img]: https://img.shields.io/hackage/v/capnp.svg
[hackage]: https://hackage.haskell.org/package/capnp
