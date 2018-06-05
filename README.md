Beginnings of a haskell [capnproto][1] library.

[![build status][ci-img]][ci]

# Status

There is a basic schema compiler plugin, which while not entirely
feature complete is capable of generating the definitions it needs
itself to operate. The generated code is currently only able to read
messages, not write them.

# License

MIT

[1]: https://capnproto.org/

[ci-img]: https://gitlab.com/isd/haskell-capnp/badges/master/build.svg
[ci]: https://gitlab.com/isd/haskell-capnp/pipelines
