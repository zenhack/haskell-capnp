This directory contains examples programs using the capnp library.

The structure of this directory tries to strike a balance between
serving as good documentation and also being usable as part of our test
suite. To serve the latter use case, the example programs are packaged
as a library; each is actually a module under `lib/`. `cmd/Main.hs` can
be used to actually run the examples.

Note that generated modules for the schema in this directory are not
checked in to revision control; you will first need to generate them by
running `../scripts/regen.sh`.
