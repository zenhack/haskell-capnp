# 0.18.0.0

- the `getBootstrap` field of `ConnConfig` has been replaced with a
  `bootstrap` field that takes the capability itself, which must now
  be constructed before spawning the connection.

# 0.17.0.0

- Get rid of the New qualifier in module paths, now that the old
  API has been removed.
- Remove references to the old API from the tutorial.
- Add two new modules:
  - `Capnp.Rpc.Membrane`, which provides helpers for implementing
    membranes.
  - `Capnp.Rpc.Revoke`, which supports revocable capabilities.

# 0.16.0.0

- Updated to work with GHC 9.2.x. In particular:
  - The generated code now enables the `FlexibleContexts` extension
    since it is no longer implied by `UndecidableInstances`
  - The `IsPtr` type alias now includes an additional constraint,
    which GHC seems to no longer be able to derive from the others.
  - Internals have been updated to deal with a breaking change to
    `template-haskell`.
- Fixed an error building the documentation.

# 0.15.0.0

- There is now a per-connection limit on the total size of incoming
  `Call` messages that are being serviced, which can be used to
  limit memory usage and provide backpressure. `ConnConfig` has
  a new `maxCallWords` field to configure this.
- Some bugs in the RPC layer have been fixed.
- `tracingTransport` now provides an option to omit call & return
  bodies from the logged messages.

# 0.14.0.0

- Significant performance improvements.
- The `Data.Mutable` module and its `Thaw` class have been replaced
  by `Capnp.Mutability` and a class `MaybeMutable`, which serves the
  same function but is easier to work with. Notably, `thaw` and `freeze
  can now be used on `Raw` values directly.
- `Mutability` is now defined in `Capnp.Mutability` instead of
  `Capnp.Message`, though the latter still re-exports it for
  compatibility (for now).
- The parameters to the `Raw` type constructor have been flipped; the
  new ordering makes it possible to implement thaw/freeze on `Raw a`
  - The `Untyped` type family has seen similar treatment.
- HasMessage and MessageDefault are now defined on a type of kind
  `* -> Mutability`, which keeps the mutability out of constraints.
  - This has the unfortunate side-effect of making type inference
    for these classes not work very well :(. Hopefully a better
    solution will be found in the future.

# 0.13.0.0

This release drops support for the old API. To upgrade to the new API,
Users should first upgrade to the latest 0.12.x version, switch their
code to use the new API, and then upgrade to this version.

# 0.12.1.0

This release backports some fixes and minor features in the new API.

# 0.12.0.0

* The 0.12.x series is the last major version that will support the old
  API; the next release will not generate code for the old API, and some
  low level interfaces will be removed. See
  <https://zenhack.net/2021/07/30/new-haskell-capnp-release-reworked-apis.html>
  for details.
* For the new API, there is a new `HasTypeId` class, with instances
  defined for all generated types.
* The new API now supports implementing RPC servers. `Capnp.Tutorial`
  discusses this, and the examples have been updated to use the new
  API.

# 0.11.0.0

* This release introduces some experimental new APIs; see the blog post
  for details: <https://zenhack.net/2021/07/30/new-haskell-capnp-release-reworked-apis.html>
  * The new APIs include support for RPC pipelining, which is not
    possible with the old APIs.
* The traversal limit is tracked at a coarser granularity. This results
  in a modest performance improvement, and may result in slightly
  different (but similar) amounts of the limit being used for a given
  computation than with the old implementation.
* Bounds on pointers are checked a bit earlier (when the pointer is
  read, rather than when its referent is accessed). As a result, some
  malformed messages may trigger bounds check errors which did not
  previously, because the offending portion of the message was not
  read.
* The limit on how many capabilities can be attached to a single message
  has been increased.
* `LimitT m` now has an instance of `MonadCatch` if `m` has an instance.
* Some harmless warnings triggered by the generated code are now
  silenced.
* `Capnp.Rpc.Promise` exposes a new function `newReadyPromise`, which
  can be used to create an already-fulfilled promise.
* Fixed a race condition where if the supervisor for a client is killed
  before the server has finished spawning, the shutdown method might
  not be run.
* `Capnp.Rpc.Server.runServer` no longer calls handleStop on exit.
  Most users of the library will not be affected, as this function is
  mostly a low-level implementation detail that is called by higher
  level functionality.
* In `Capnp.Message` some uses of the type `Int` in the API have been
  strengthened to type `WordCount`.

# 0.10.0.1

* Fix a bug causing spurious exceptions when creating very large
  messages.

# 0.10.0.0

* The parametrization of messages has been reworked. Most things that
  were parametrized over a message type are now parametrized over a
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
