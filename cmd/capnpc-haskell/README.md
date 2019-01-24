This directory contains the code generator plugin, which is run when you
do `capnp compile -o haskell`. This document provides a high-level
overview of its internals.

Like all capnproto code generator plugins, it accepts a
capnproto-serialized `CodeGenratorRequest` (defined in `schema.capnp`)
on standard input, and emits code accordingly. For an overview on *what*
exactly is emitted, see `Capnp.Tutorial`.

The data read from standard input transitions through several
intermediate forms before finally being written out to disk as Haskell
source code. The `IR/` subdirectory defines the intermediate forms
themselves, while each transition is defined in a module under `Trans/`.
`Main` ties ties the whole thing together.

# The Pipeline


```
                               +---> Raw ---+
                               |            |
CGR ----> Stage1 ----> Flat ---+            +---> Haskell (IR) --> Haskell (source code)
                               |            |
                               +--> Pure ---+
```

The flow is as follows:

* First, we read in the data from standard in, and parse it as a
  `CodeGeneratorRequest` (henceforth `CGR`). We use the high-level API
  for this.
* Next, we translate to the IR defined in `IR.Stage1`. This represents
  essentially the same information as the CGR, but:
  * Information we won't use has been discarded.
  * It is more type safe, in that the structure of the IR cannot
    represent certain illegal structures that are representable in the
    CGR.
  * Whereas the CGR contains a list of nodes by id, and entries in the
    CGR reference each other by ID, in the Stage1 IR we have tied the
    knot, and the referenced nodes are available directly as fields of
    their referers.
* Then, we translate to the `Flat` IR. This is still a declarative
  representation of the information in the schema, but it is in a form
  that maps more nicely to Haskell. The goal of this stage is to massage
  the information into a form such that later stages don't have to do any
  work to find out basic facts about the schema.
  For example:
  * We have flattened the namespace, all definitions are at top level;
    Haskell doesn't support nested namespaces, so this solves an
    impedence mismatch. The hierarchical structure is recoverable, as
    the names themselves encode the structure; see `IR.Name`.
  * Unions are now represented more cleanly as sums, rather than being
    mixied in with other fields.
  * Interfaces now contain a full list of their ancestors, not just
    immediate superclasses. This is useful when genrating instance
    declarations later on.
* Next, the pipeline splits in two; we translate the Flat IR into
  the 'Raw' IR, which from which we derive the low-level API modules,
  and the 'Pure' IR, from which we derive the high-level API modules.
  Each of these forms encodes the Haskell structures to be generated,
  rather than a declarative representation of the schema.
  * The Raw IR contains things like:
    * getter for this field on this type
    * newtype wrapper for a struct with this name
    * etc.
  * The Pure IR contains things like:
    * declaration for a product type with these fields
    * an instance of 'Decerialize' for this type
    * etc.
* Then, we translate each of these forms into a Haskell AST, defined
  in `IR.Haskell`. We use our own AST instead of an off-the-shelf
  library for a couple reasons:
  * We want to eventually insert comments from the schema into the
    code as Haddock comments, and the libraries I(zenhack) was able
    to find to not support comments.
  * The full Haskell syntax is quite complicated, and we don't need
    most of it; our AST has just the parts we need, and so is simpler
    to work with.
  * It is useful when generating code to be able treat certain
    constructs as primitive syntax which really aren't. For example:
    * Our AST does not reflect the fact that function application is
      curried;
    * Our expression type treats "functorial" application, e.g.
      `f <$> x <*> y <*> z`, as primitive, which is convienent for
      our purposes.
* Finally, we translate the Haskell AST into text, and write it out
  to disk.
