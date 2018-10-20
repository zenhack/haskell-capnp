# 0.4.0.0

* Fix a bug where `defaultLimit` was actually much larger than
  documented.
* Fix a bug where reads on `Handle`s at EOF would cause a call to error.
* Beginnings of RPC support; the API will likely change substantially.
* Some changes to the module hierarchy:
  * Generated code is now placed under `Capnp.Gen`, rather than `Capnp`.
  * The `Data` prefix has been removed from the `Data.Capnp` hierarchy.
* `createPure` can now be used with any instance of `MonadThrow`, not
  just `Either SomeException`.

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

[issue41]: https://github.com/zenhack/haskell-capnp/issues/41
[issue34]: https://github.com/zenhack/haskell-capnp/issues/34
