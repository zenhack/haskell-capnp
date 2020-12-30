
# 0.11.0.0

* The traversal limit is tracked at a coarser granularity. This results
  in a modest performance improvement, and may result in slightly
  different (but similar) amounts of the limit being used for a given
  computation than with the old implementation.

# 0.10.0.0

* The parametrization of messages has been reworked. Most things that
  per parametrized over a message type are now parametrized over a
  type parameter of kind `Mutability` (defined in `Capnp.Message`).
  * The Message type class has been replaced with a type `Message` of
    kind `Mutability -> *`, and a type class `MonadReadMessage` for
    operations that can work on any Message.
* Message.fromByteString/toByteString:
  * are now stand-alone functions, rather than methods on the class
  * only operate on immutable segments.
  * are pure functions, rather than monadic ones.
* Some functions that were derivative of
  `toByteString`/`fromByteString`, e.g. `Untyped.rawBytes`, also only
  work on immutable messages now, and may be pure functions where before
  they were monadic.
* The types defined in `Capnp.Basics` now have `Thaw` instances.

# 0.9.0.0

* Significant performance improvements. A few low level APIs have been
  changed to support this work.

# 0.8.0.0

## Serialization

* The code generator now supports type parameters in schema; previously
  the code generator would treat these the same as AnyPointer, but now
  the generated Haskell has corresponding type parameters.
* The `Cerialize` and `Marshal` type classes now take the state token
  `s` as a parameter, which was necessary to support type parameters.

## Misc

* Some of the multi-parameter type classes in Capnp.Classes now have
  functional dependencies between their parameters. This should
  generally improve type inference.

# 0.7.0.0

## Serialization

* The library now supports canonicalization via Capnp.canonicalize
* There is a new function Capnp.Message.singleSegment for constructing a
  `ConstMsg` from an (unframed) `Segment ConstMsg`.

## RPC

* Some signatures in Capnp.Rpc.Untyped have changed to facilitate a
  future release supporting promise pipelining.

## Misc

* The included schema now match those in version 0.8 of the reference
  implementation

# 0.6.0.3

* Fix a race condition [(#74)][issue74] causing remote objects to very
  occasionally be released too early.

# 0.6.0.2

* Fix a bug [(#71)][issue71] which sometimes caused the code generator
  to crash.

# 0.6.0.1

* Fix a bug in the code generator [(#72)][issue72] which sometimes
  resulted in build failures of the generated code.

# 0.6.0.0

## RPC

* Fix a serious bug in `newPromiseClient`, resulting in dropped calls
  made on the promise before it is resolved.
* There is now a `Server` class, which all RPC servers must implement.
  All of its methods have default implementations, so adding an instance
  to existing servers is straightforward.
* It is now possible to "unwrap" clients that point to a local server
  using the new 'Capnp.Rpc.unwrapServer' function, if the server
  implements support for it with the new 'Server' type class.
* Servers can now specify a hook to be run when the server is shut down,
  using the server class's 'shutdown' method.

# 0.5.0.0

## Serialization

In `Capnp.Untyped`, The `dataSection` and `ptrSection` APIs have been
removed. `structDataSize` has been replaced with `structWordCount`,
and new functions `structByteCount and `structPtrCount` have been added.

## RPC

This release brings some improvements to the RPC API:

* Add `newPromiseClient` for creating a promise-based client that can
  be fulfilled later.
* Make `Client` an instance of `IsClient`.
* Previously, there were a number of functions that had two variants:
  * `foo`, which ran in `IO`
  * `fooSTM`, which ran in `STM`.

  Now there is just one variant, `foo`, which is polymorphic over
  `MonadSTM`, defined by the `monad-stm` package. There are instances of
  this class for `IO`, `STM`, and any monad transformer applied to an
  instance.

  Note that previously some of the `foo` variants were polymorphic over
  `MonadIO`. Unfortunately it is not possible to define a general instance
  `MonadIO m => MonadSTM m`, but for a particular mtl stack that
  has an instance of `MonadIO`, you can fix the problem by defining:

  ```haskell
  instance MonadSTM MyStack where
      liftSTM = liftIO . liftSTM
  ```

  Or, you can add `liftIO`/`liftSTM` to the appropriate call sites.

# 0.4.0.0

* RPC support! This should be considered alpha quality for now. The API
  will likely change substantially.
* Many bug fixes; users are strongly encouraged to upgrade.
* Reorganization of the module hierarchy:
  * Generated code is now placed under `Capnp.Gen`, rather than `Capnp`.
  * The `Data` prefix has been removed from the `Data.Capnp` hierarchy.
* The included generated modules for the core schema have been updated
  to those shipped with version 0.7 of the reference implementation.
* Other miscellaneous API Changes:
  * `createPure` can now be used with any instance of `MonadThrow`, not
    just `Either SomeException`.
  * `LimitT m` is now an instance of `MonadIO`, provided that `m` is an
    instance.
  * More type class instances from elsewhere in the library are
    re-exported via the `Capnp` module.
  * The `IsPtr` type class has been split into `FromPtr` and `ToPtr`. Most
    user code should not care about this.
  * Generated high-level types no longer have Read instances; interfaces
    make this problematic.
  * Getters for anonymous unions are now `get_Foo'` instead of
    `get_Foo'union'`.
  * `newMessage` now accepts an optional size hint.
  * Instances of `Cerialize` now exist/are generated for
    `(Vector (Vector (Vector ...)))` up to a reasonable depth.
* Other improvements not directly reflected in the API:
  * The allocation strategy has changed to reduce unnecessary copying.
  * It is now possible to create messages with a size > 2GiB. Note that
    individual segments are still limited.

# 0.3.0.0

* Instances of some type classes are no longer generated for "second
  class" types (i.e. types which cannot appear as stand-alone
  definitions in schema files -- groups and unions).
* `has_*` functions are now only generated for pointer fields.
* Various non-functional changes in the output of the code generator.
* We now generate constants for (most) pointer types; previously
  constants defined in a schema would not result in any generated code
  ([#41][issue41]).
* The `set_*` functions now check if the arguments are in the same
  message, and copy if need be ([#34][issue34]).
* `MutMsg` is now an instance of `Eq`.
* The `HasMessage` class from `Data.Capnp.Untyped` is now a type family,
  rather than a multi-parameter type class. This improves inference and
  removes some superfluous generalization.
* The module `Data.Capnp.Pure` has been folded into `Data.Capnp`. If you
  were previously using the `Text` and `Data` type aliases it exported,
  you should instead use `Text` from `Data.Text` and `ByteString` from
  `Data.ByteString`; the `Text` and `Data` exported by `Data.Capnp` are
  types from the low-level API.

# 0.2.0.0

* Redesign the 'Mutable' type class's API.
* Provide helpers for doing zero-copy message creation in pure code.
* General improvements to the documentation.

# 0.1.0.0

* First release; basic read & write support, serialization only.

[issue34]: https://github.com/zenhack/haskell-capnp/issues/34
[issue41]: https://github.com/zenhack/haskell-capnp/issues/41
[issue71]: https://github.com/zenhack/haskell-capnp/issues/71
[issue72]: https://github.com/zenhack/haskell-capnp/issues/72
