[![hackage][hackage-img]][hackage]

A Haskell library for the [Cap'N Proto][1] Cerialization and RPC
protocol.

# Getting Started

There is a module `Capnp.Tutorial` which contains an introduction
to the library; users are *strongly* encouraged to read this first, as
the reference documentation can be bewildering without that context.

# Status

Serialization support works, with some limitations:

* We do not support defining custom default values for fields of pointer
  type; see ([#28][issue28]).
* We currently do not correctly handle decoding lists of structs from
  non-composite lists ([#27][issue27]). This means that, contrary to the
  [protocol evolution rules][2], it is not safe to change a field from
  type List(T) (where T is any non-struct type) to a list of a struct
  type.

Level 1 RPC support is implemented and usable, though it should be
considered alpha quality for now. Specific things to be aware of:

* The implementation is *not* robust against resource exhaustion
  attacks; for now users are strongly discouraged from using it to do
  RPC with untrusted peers.

The API is considered unstable. It will likely see changes, for the
sake of polish, consistency, etc. as well as to improve performance and
accommodate more features as we add them.

[1]: https://capnproto.org/
[2]: https://capnproto.org/language.html#evolving-your-protocol

[issue27]: https://github.com/zenhack/haskell-capnp/issues/27
[issue28]: https://github.com/zenhack/haskell-capnp/issues/28

[hackage-img]: https://img.shields.io/hackage/v/capnp.svg
[hackage]: https://hackage.haskell.org/package/capnp
