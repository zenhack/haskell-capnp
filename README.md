A Haskell [capnproto][1] library.

[![build status][ci-img]][ci]

# Status

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

The API is considered unstable. It will likely see changes, for the
sake of polish, consistency, etc. as well as to improve performance and
accommodate more features as we add them (RPC in particular will
probably require changing some interfaces).

# Overview

The API is roughly divided into two parts: a low level API and a high
level API. The high level API eschews some of the benefits of the wire
format in favor of a more convenient interface.

## High Level API

The high level API exposes capnproto values as regular algebraic data
types.

On the plus side:

* This makes it easier to work with capnproto values using idiomatic
  Haskell code
* Because we have to parse the data up-front we can *validate* the data
  up front, so (unlike the low level API), you will not have to deal with
  errors while traversing the message.

Both of these factors make the high level API generally more pleasant
to work with and less error-prone than the low level API.

The downside is that you can't take advantage of some of the novel
properties of the wire format. In particular:

* It is theoretically slower, as there is a marshalling step involved
  (actual performance has not been measured).
* You can't mmap a file and read in only part of it.
* You can't modify a message in-place.

## Example

As a running example, we'll use the following schema (borrowed from the
C++ implementation's documentation):

```capnp
# addressbook.capnp
@0xcd6db6afb4a0cf5c;

struct Person {
  id @0 :UInt32;
  name @1 :Text;
  email @2 :Text;
  phones @3 :List(PhoneNumber);

  struct PhoneNumber {
    number @0 :Text;
    type @1 :Type;

    enum Type {
      mobile @0;
      home @1;
      work @2;
    }
  }

  employment :union {
    unemployed @4 :Void;
    employer @5 :Text;
    school @6 :Text;
    selfEmployed @7 :Void;
    # We assume that a person is only one of these.
  }
}

struct AddressBook {
  people @0 :List(Person);
}
```

Once the `capnp` and `capnpc-haskell` executables are installed and in
your `$PATH`, you can generate code for this schema by running:

```
capnp compile -ohaskell addressbook.capnp
```

This will create the following files relative to the current directory:

* `Capnp/Addressbook.hs`
* `Capnp/Addressbook/Pure.hs`
* `Capnp/ById/Xcd6db6afb4a0cf5c/Pure.hs`
* `Capnp/ById/Xcd6db6afb4a0cf5c.hs`

The modules under `ById` are an implementation detail.
`Capnp/Addressbook.hs` is generated code for use with the low level API.
`Capnp/Addressbook/Pure.hs` is generated code for use with the high
level API. It will export the following data declarations (cleaned up
for readability).

```haskell
module Capnp.Addressbook where

import Data.Int
import Data.Text   (Text)
import Data.Vector (Vector)
import Data.Word

data AddressBook = AddressBook
    { people :: Vector Person
    }

data Person = Person
    { id         :: Word32
    , name       :: Text
    , email      :: Text
    , phones     :: Vector Person'PhoneNumber
    , employment :: Person'employment
    }

data Person'PhoneNumber = Person'PhoneNumber
    { number :: Text
    , type_  :: Person'PhoneNumber'Type
    }

data Person'employment
    = Person'employment'unemployed
    | Person'employment'employer Text
    | Person'employment'school Text
    | Person'employment'selfEmployed
    | Person'employment'unknown' Word16

data Person'PhoneNumber'Type
    = Person'PhoneNumber'Type'mobile
    | Person'PhoneNumber'Type'home
    | Person'PhoneNumber'Type'work
    | Person'PhoneNumber'Type'unknown' Word16
```

Note that we use the single quote character as a namespace separator for
namespaces within a single capnproto schema. I(@zenhack) generally
advise against deeply nested namespaces; in languages which can't define
hierarchical and/or mutually recursive modules they usually end up
generating excessively long names. For the example schema I would suggest
moving the definition of the `PhoneNumber` struct outside of `Person`,
which results in:

The module also exports instances of several type classes:

* `Show`
* `Read`
* `Eq`
* `Generic` from `GHC.Generics`
* `Default` from the `data-default` package.
* A number of type classes defined by the `capnp` package.
* Capnproto enums additionally implement the `Enum` type class.

Using the `Default` instance to construct values means that your
existing code will continue to work if new fields are added in the
schema, but it also makes it easier to forget to set a field if you had
intended to. The instance maps `def` to the default value as defined by
capnproto, so leaving out newly-added fields will do The Right Thing.

```haskell
-- ...

data Person = Person
    { id         :: Word32
    , name       :: Text
    , email      :: Text
    , phones     :: Vector PhoneNumber
    , employment :: Person'employment
    }

data PhoneNumber = PhoneNumber
    { number :: Text
    , type_  :: PhoneNumber'Type
    }

data PhoneNumber'Type
    = PhoneNumber'Type'mobile
    | PhoneNumber'Type'home
    | PhoneNumber'Type'work
    | PhoneNumber'Type'unknown' Word16

-- ...
```

The module `Data.Capnp.Pure` exposes the most frequently used
functionality from the high level API. We can output an address book
message to standard output like so:

```haskell
{-# LANGUAGE OverloadedStrings     #-}
-- Note that DuplicateRecordFields is usually needed, as the generated
-- code relys on it to resolve collisions in capnproto struct field
-- names:
{-# LANGUAGE DuplicateRecordFields #-}
import Capnp.Addressbook

-- Note that Data.Capnp.Pure re-exports `def`, as a convienence
import Data.Capnp.Pure (putValue, def)

import qualified Data.Vector as V

main = putValue AddressBook
    { people = V.fromList
        [ Person
            { id = 123
            , name = "Alice"
            , email = "alice@example.com"
            , phones = V.fromList
                [ def
                    { number = "555-1212"
                    , type_ =  Person'PhoneNumber'mobile
                    }
                ]
            , employment = Person'employment'school "MIT"
            }
        , Person
            { id = 456
            , name = "Bob"
            , email = "bob@example.com"
            , phones = V.fromList
                [ def
                    { number = "555-4567"
                    , type_ = Person'PhoneNumber'Type'home
                    }
                , def
                    { number = "555-7654
                    , type_ = Person'PhoneNumber'Type'work
                    }
                ]
            }
        ]
    }

```

`putValue` is equivalent to `hPutValue stdout`; `hPutValue` may be used
to write to an arbitrary handle.

We can use `getValue` (or alternately `hGetValue`) to read in a message:

```haskell
-- ...

import Data.Capnp.Pure (getValue, defaultLimit)

-- ...

main = do
    value <- getValue defaultLimit
    print (value :: AddressBook)
```

Note the type annotation; there are a number of interfaces in the
library which dispatch on return types, and depending on how they are
used you may have to give GHC a hint for type inference to succeed.

`defaultLimit` is a default value for the traversal limit, which acts to
prevent denial of service vulnerabilities; See the documentation in
`Data.Capnp.TraversalLimit` for more information. `getValue` uses this
argument both to catch values that would cause excessive resource usage,
and to simply limit the overall size of the incoming message. The
default is approximately 64 MiB.

## Code generation rules.

The complete rules for how capnproto types map to Haskell are as follows:

* Integer types and booleans map to the obvious corresponding Haskell
  types.
* `Float32` and `Float64` map to `Float` and `Double`, respectively.
* `Void` maps to the unit type, `()`.
* Lists map to `Vector`s from the Haskell `vector` package. Note that
  right now we use boxed vectors for everything; at some point this will
  likely change for performance reasons. Using the functions from
  `Data.Vector.Generic` will probably decrease the amount of code you
  will need to modify when upgrading.
* `Text` maps to (strict) `Text` from the Haskell `text` package.
* `Data` maps to (strict) `ByteString`s
* Type constructor names are the fully qualified (within the schema file)
  capnproto name, using the single quote character as a namespace
  separator.
* Structs map to record types. The name of the data constructor is the
  same as the name of the type constructor.
* Field names map to record fields with the same names. Names that are
  Haskell keywords have an underscore appended to them, e.g. `type_` in
  the above example. These names are not qualified; we use the
  `DuplicateRecordFields` extension to disambiguate them.
* Union fields result in an auxiliary type definition named
  `<containing type's name>'<union field name>`. For an example, see the
  mapping of the `employment` field above.
* Unions and enums map to sum types, each of which has a special
  `unknown'` variant (note the trailing single quote). This variant will
  be returned when parsing a message which contains a union tag greater
  than what was defined in the schema. This is most likely to happen
  when dealing with data generated by software using a newer version
  of the same schema.
* Union variants with arguments of type `Void` map to data constructors
  with no arguments.
* The type for an anonymous union has the same name as its containing
  struct with an extra single quote on the end. You can think of this as
  being like a field with the empty string as its name. The Haskell
  record accessor for this field is named `union'` (note the trailing
  single quote).
* As a special case, if a struct consists entirely of one anonymous
  union, the type for the struct itself is omitted, and the name of the
  type for the union does not have the trailing single quote (so its
  name is what the name of the struct type would be).
* No code is currently generated for interfaces; this will change once
  we implement RPC.

# License

MIT

[1]: https://capnproto.org/
[2]: https://capnproto.org/language.html#evolving-your-protocol

[issue27]: https://github.com/zenhack/haskell-capnp/issues/27
[issue28]: https://github.com/zenhack/haskell-capnp/issues/28
[issue29]: https://github.com/zenhack/haskell-capnp/issues/29

[ci-img]: https://gitlab.com/isd/haskell-capnp/badges/master/build.svg
[ci]: https://gitlab.com/isd/haskell-capnp/pipelines
