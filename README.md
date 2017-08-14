Beginnings of a haskell [capnproto][1] library.

# Status

We have partial support for reading and building untyped messages (i.e.
working at the level of structs/lists/pointers, with no schema).
Actual codegen is being planned. Lots of stuff still doesn't work; We
don't handle far pointers *at all*, and there are some other edge cases
in the readers. The builders are even more immature.

Stay tuned.

# License

MIT

[1]: https://capnproto.org/
