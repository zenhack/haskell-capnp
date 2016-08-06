Beginnings of a haskell [capnproto][1] library. No promises about
following through on this one.

Have some of the schema datatype (manually) spec'd out, plus a type for
messages (array of array of segments).

I don't actually feel like I know what I'm doing when it comes to
writing serialization libraries for haskell; if my design is crap,
please tell me (politely).

Basic plan is this:

* For read, load each segment into an array. As far as actually
  converting to datatypes, do this lazily. Questions: Can we get
  shared-memory manipulation working? Would need to pin the pointer. Do
  we want to?
* For write... Not as sure. Try to mirror the above somehow?
* Use ast datatypes from Template Haskell, this way we can generate
  source files or just invoke capnp compile from the build system.

Haven't thought a whole lot about rpc yet.

[1]: https://capnproto.org/
