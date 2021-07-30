-- |
-- Module: Capnp.Tutorial
-- Description: Tutorial for the Haskell Cap'N Proto library.
--
-- This module provides a tutorial on the overall usage of the library. Note that
-- it does not aim to provide a thorough introduction to capnproto itself; see
-- <https://capnproto.org> for general information.
--
-- Each of the example programs described here can also be found in the @examples/@
-- subdirectory in the source repository.
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Capnp.Tutorial (
    -- * Overview
    -- $overview

    -- * Setup
    -- $setup

    -- * API Transition
    -- $api_transition

    -- * New API
    -- ** Serialization

    -- * Old API
    -- ** Serialization
    -- $serialization

    -- *** High Level API
    -- $highlevel

    -- **** Example
    -- $highlevel-example

    -- **** Code Generation Rules
    -- $highlevel-codegen-rules

    -- *** Low Level API
    -- $lowlevel

    -- **** Example
    -- $lowlevel-example

    -- **** Write Support
    -- $lowlevel-write

    -- ** RPC
    -- $rpc
    ) where

-- So haddock references work:
import System.IO (stdout)

import qualified Data.ByteString as BS
import qualified Data.Text       as T

import Capnp

import Capnp.Classes (FromStruct)

-- $overview
--
-- This module provides an overview of the capnp library.

-- $setup
--
-- In order to generate code from schema files, you will first need to make
-- sure the @capnp@ and @capnpc-haskell@ binaries are in your @$PATH@. The
-- former ships with the capnproto reference implementation; see
-- <https://capnproto.org/install.html>. The latter is included with this
-- library; to install it you can run the command:
--
-- > cabal install capnp --installdir=$DIR
--
-- which will compile the package and create the @capnpc-haskell@ executable
-- at @$DIR/capnpc-haskell@.

-- $api_transition
--
-- This package is in them midst of transitioning many existing APIs over
-- to a new design. As such, in this tuotrial we refer to the new api and the
-- old API.
--
-- The old API will eventually be removed, but not before there is at least
-- one release where both APIs are present and the new API has reached feature
-- pairty. Right now, the primary missing functionality is in implementing
-- RPC servers (clients work fine, better even).
--
-- This tutorial only covers the new API, but the tutorial for the old APIs
-- is still available (and still correct) in the documentation for version
-- 0.10 of this package: https://hackage.haskell.org/package/capnp-0.10.0.1
--
-- For more information about the reasons behind the new API, see:
-- <http://zenhack.net/TODO>. TODO: link to blog post.


-- $serialization
--
-- The serialization API is roughly divided into two parts: a low level API
-- and a high level API. The high level API eschews some of the benefits of
-- the wire format in favor of a more convenient interface.

-- $highlevel
--
-- The high level API exposes capnproto values as regular algebraic data
-- types.
--
-- On the plus side:
--
-- * This makes it easier to work with capnproto values using idiomatic
--   Haskell code
-- * Because we have to parse the data up-front we can *validate* the data
--   up front, so (unlike the low level API), you will not have to deal with
--   errors while traversing the message.
--
-- Both of these factors make the high level API generally more pleasant
-- to work with and less error-prone than the low level API.
--
-- The downside is that you can't take advantage of some of the novel
-- properties of the wire format. In particular:
--
-- * It is slower, as there is a marshalling step involved, and it uses more
--   memory.
-- * You can't mmap a file and read in only part of it.
-- * You can't modify a message in-place.

-- $highlevel-example
--
-- As a running example, we'll use the following schema (borrowed from the
-- C++ implementation's documentation):
--
-- > # addressbook.capnp
-- > @0xcd6db6afb4a0cf5c;
-- >
-- > struct Person {
-- >   id @0 :UInt32;
-- >   name @1 :Text;
-- >   email @2 :Text;
-- >   phones @3 :List(PhoneNumber);
-- >
-- >   struct PhoneNumber {
-- >     number @0 :Text;
-- >     type @1 :Type;
-- >
-- >     enum Type {
-- >       mobile @0;
-- >       home @1;
-- >       work @2;
-- >     }
-- >   }
-- >
-- >   employment :union {
-- >     unemployed @4 :Void;
-- >     employer @5 :Text;
-- >     school @6 :Text;
-- >     selfEmployed @7 :Void;
-- >     # We assume that a person is only one of these.
-- >   }
-- > }
-- >
-- > struct AddressBook {
-- >   people @0 :List(Person);
-- > }
--
-- Once the @capnp@ and @capnpc-haskell@ executables are installed and in
-- your @$PATH@ (see the Setup section above), you can generate code for
-- this schema by running:
--
-- > capnp compile -ohaskell addressbook.capnp
--
-- This will create the following files relative to the current directory:
--
-- * Capnp\/Gen\/Addressbook.hs
-- * Capnp\/Gen\/Addressbook\/Pure.hs
-- * Capnp\/Gen\/Addressbook\/New.hs
-- * Capnp\/Gen\/ById\/Xcd6db6afb4a0cf5c/Pure.hs
-- * Capnp\/Gen\/ById\/Xcd6db6afb4a0cf5c/New.hs
-- * Capnp\/Gen\/ById\/Xcd6db6afb4a0cf5c.hs
--
-- The modules under @ById@ are an implementation detail.
-- @Capnp\/Gen\/Addressbook\.New.hs@ is generated code for use with the new API.
-- Other files are for use with the old API, and not covered here.
--
-- The generated moule will export declarations like the following (cleaned up
-- and abbreviated for readability):
--
-- > import qualified Capnp.Repr as R
-- > import qualified Capnp.Classes.New as C
-- > import qualified Capnp.Repr.Parsed as RP
-- > import GHC.Generics (Generic)
-- >
-- > data Person
-- >
-- > type instance (R.ReprFor Person) = R.Ptr (Just R.Struct)
-- >
-- > instance (C.TypedStruct Person) where { ... }
-- > instance (C.Allocate Person) where { ... }
-- >
-- > data instance C.Parsed Person
-- >     = Person
-- >         { id :: Word32
-- >         , name :: RP.Parsed Basics.Text
-- >         , email :: RP.Parsed Basics.Text
-- >         , phones :: RP.Parsed (R.List Person'PhoneNumber)
-- >         , employment :: RP.Parsed Person'employment
-- >         }
-- >     deriving(Generic, Show, EQ)
-- >
-- > instance HasField "id" Slot Person Std_.Word32 where { ... }
-- > instance HasField "name" Slot Person Basics.Text where { ... }
-- > instance HasField "email" Slot Person Basics.Text where { ... }
-- > instance HasField "phones" Slot Person (R.List Person'PhoneNumber) where { ... }
-- > instance HasField "employment" Group Person Person'employment where { ... }
--
-- > data Person'employment
-- >
-- > type instance R.ReprFor Person'employment = R.Ptr (Std_.Just R.Struct)
-- > instance C.TypedStruct Person'employment where { ... }
-- > instance C.Allocate Person'employment where { ... }
--
-- > data instance C.Parsed Person'employment
-- >     = Person'employment'
-- >         { union' :: C.Parsed (GH.Which Person'employment)
-- >         }
-- >     deriving(Generic, Show, Eq)
-- >
-- > instance (GH.HasUnion Person'employment) where
-- >     unionField = ...
-- >     data RawWhich mut_ Person'employment
-- >         = RW_Person'employment'unemployed (R.Raw mut_ ())
-- >         | RW_Person'employment'employer (R.Raw mut_ Basics.Text)
-- >         | RW_Person'employment'school (R.Raw mut_ Basics.Text)
-- >         | RW_Person'employment'selfEmployed (R.Raw mut_ ())
-- >         | RW_Person'employment'unknown' Word16
-- >     data Which Person'employment
-- >
-- > instance GH.HasVariant "unemployed" GH.Slot Person'employment () where { ... }
-- > instance GH.HasVariant "employer" GH.Slot Person'employment Basics.Text where { ... }
-- > instance GH.HasVariant "school" GH.Slot Person'employment Basics.Text where { ... }
-- > instance GH.HasVariant "selfEmployed" GH.Slot Person'employment () where { ... }
-- >
-- > data instance C.Parsed (Which Person'employment)
-- >     = Person'employment'unemployed
-- >     | Person'employment'employer (RP.Parsed Basics.Text)
-- >     | Person'employment'school (RP.Parsed Basics.Text)
-- >     | Person'employment'selfEmployed
-- >     | Person'employment'unknown' Std_.Word16
-- >     deriving(Generic, Show, Eq)
-- >
-- > instance C.Parse (GH.Which Person'employment) (C.Parsed (GH.Which Person'employment)) where
-- >     ...
-- >
-- > data Person'PhoneNumber
-- >
-- > type instance R.ReprFor Person'PhoneNumber = R.Ptr (Std_.Just R.Struct)
-- >
-- > ...
-- >
-- > data Person'PhoneNumber'Type
-- >     = Person'PhoneNumber'Type'mobile
-- >     | Person'PhoneNumber'Type'home
-- >     | Person'PhoneNumber'Type'work
-- >     | Person'PhoneNumber'Type'unknown' Std_.Word16
-- >     deriving(Generic, Eq, Show)
-- >
-- > type instance R.ReprFor Person'PhoneNumber'Type = R.Data R.Sz16
-- >
-- > instance Enum Person'PhoneNumber'Type where { ... }
-- >
-- > ...
--
-- Note that we use the single quote character as a namespace separator for
-- namespaces within a single capnproto schema.
--
-- So, we see that capnpc-haskell generates:
--
-- * For each struct type or group:
--   * An uninhabited type corresponding to that type
--   * An instance of the 'R.ReprFor' type family, marking the type as having
--     a struct as its representation.
--   * An instance of 'HasField' for each field in the struct.
--   * An instance of the 'C.Parsed' data family, which is an idiomatic Haskell
--     ADT corresponding to the structure of the capnproto type.
--     * If the struct has an anonymous union, some instances related to this,
--       including a data family instance for @'Parsed' ('Which' a)@, which
--       is an ADT representation of the union. Note that there is an @unknown'@
--       variant, which is used for variants found on the wire that are not known
--       to the schema (usually because the value was constructed using a newer
--       version of the schema).
-- * For each enum:
--   * An ADT corresponding to that enum. There is no uninhabited type, and no
--     'C.Parsed' data family instance; the type itself serves as both. As with
--     unions, there is an @unknown'@ variant for unrecognized variants.
--   * An instance of 'R.ReprFor', recording the wire representation of the enum
--     (always 16-bit).
--
-- Some additional things are generated for interfaces, but we cover those
-- in the RPC section below.
--
-- The module "Capnp.New" exposes the most frequently used
-- functionality from the capnp package. We can write an address book
-- message to standard output using the high-level API like so:
--
-- > {-# LANGUAGE OverloadedStrings     #-}
-- > -- Note that DuplicateRecordFields is usually needed, as the generated
-- > -- code relys on it to resolve collisions in capnproto struct field
-- > -- names:
-- > {-# LANGUAGE DuplicateRecordFields #-}
-- > import Capnp.Gen.Addressbook.New
-- >
-- > -- Note that Capnp.New re-exports `def`, as a convienence
-- > import Capnp.New (putParsed, def)
-- >
-- > import qualified Data.Vector as V
-- >
-- > main = putParsed AddressBook
-- >     { people = V.fromList
-- >         [ Person
-- >             { id = 123
-- >             , name = "Alice"
-- >             , email = "alice@example.com"
-- >             , phones = V.fromList
-- >                 [ def
-- >                     { number = "555-1212"
-- >                     , type_ =  Person'PhoneNumber'Type'mobile
-- >                     }
-- >                 ]
-- >             , employment = Person'employment $ Person'employment'school "MIT"
-- >             }
-- >         , Person
-- >             { id = 456
-- >             , name = "Bob"
-- >             , email = "bob@example.com"
-- >             , phones = V.fromList
-- >                 [ def
-- >                     { number = "555-4567"
-- >                     , type_ = Person'PhoneNumber'Type'home
-- >                     }
-- >                 , def
-- >                     { number = "555-7654"
-- >                     , type_ = Person'PhoneNumber'Type'work
-- >                     }
-- >                 ]
-- >             , employment = Person'employment $ Person'employment'selfEmployed
-- >             }
-- >         ]
-- >     }
--
-- 'putValue' is equivalent to @'hPutValue' 'stdout'@; 'hPutValue' may be used
-- to write to an arbitrary handle.
--
-- We can use 'getParsed' (or alternately 'hGetParsed') to read in a message:
--
-- > -- ...
-- >
-- > {-# LANGUAGE TypeApplications #-}
-- > import Capnp.New (getParsed, defaultLimit)
-- >
-- > -- ...
-- >
-- > main = do
-- >     value <- getParsed @AddressBook defaultLimit
-- >     print value
--
-- Note the use of @TypeApplications@; there are a number of interfaces in the
-- library which dispatch on return types, and depending on how they are
-- used you may have to give GHC a hint for type inference to succeed.
--
-- The type of 'getParsed' is:
--
-- @'getParsed' :: (R.IsStruct a, Parse a pa) => WordCount -> IO pa
--
-- ...and so it may be used to read in any struct type.
--
-- 'defaultLimit' is a default value for the traversal limit, which acts to
-- prevent denial of service vulnerabilities; See the documentation in
-- "Capnp.TraversalLimit" for more information. 'getValue' uses this
-- argument both to catch values that would cause excessive resource usage,
-- and to simply limit the overall size of the incoming message. The
-- default is approximately 64 MiB.
--
-- If an error occurs, an exception will be thrown of type 'Error' from the
-- "Capnp.Errors" module.

-- $lowlevel
--
-- The low level API exposes a much more imperative interface than the
-- high-level API. Instead of algebraic data types, There is an opaque
-- wrapper type 'R.Raw':
--
-- @
-- newtype Raw (mut :: Mutability) a = ...
-- @
--
-- which accepts as type parameters the mutability of the underlying message,
-- and a phantom type indicating the capnproto type. This second type parameter
-- will be instantiated with the (for structs, uninhabited) type generated by
-- the schema compiler plugin. The accessors in "Capnp.New.Accessors"
-- (re-exported by "Capnp.New") are used to read and write the fields.
-- This API is much closer in spirit to that of the C++ reference implementation.
--
-- Because the low level interfaces do not parse and validate the message
-- up front, accesses to the message can result in errors. Furthermore, the
-- traversal limit needs to be tracked to avoid denial of service attacks.
--
-- Because of this, access to the message must occur inside of a monad
-- which is an instance of `MonadThrow` from the exceptions package, and
-- `MonadLimit`, which is defined in "Capnp.TraversalLimit". We define
-- a monad transformer `LimitT` for the latter.

-- $lowlevel-example
--
-- We'll use the same schema as above for our example.
--
-- @
-- newtype AddressBook msg = ...
--
-- get_Addressbook'people :: ReadCtx m msg => AddressBook msg -> m (List msg (Person msg))
--
-- newtype Person msg = ...
--
-- get_Person'id   :: ReadCtx m msg => Person msg -> m Word32
-- get_Person'name :: ReadCtx m msg => Person msg -> m (Text msg)
-- @
--
-- `ReadCtx` is a type synonym:
--
-- @
-- type ReadCtx m msg = (Message m msg, MonadThrow m, MonadLimit m)
-- @
--
-- Note the following:
--
-- * The generated data types are parametrized over a `msg` type. This is
--   the type of the message in which the value is contained. This can be
--   either 'ConstMsg' in the case of an immutable message, or @'MutMsg' s@
--   for a mutable message (where `s` is the state token for the monad in
--   which the message may be mutated).
-- * The `Text` and `List` types mentioned in the type signatures are types
--   defined within the capnp library, and are similarly views into the
--   underlying message.
-- * Access to the message happens in a monad which affords throwing
--   exceptions, tracking the traversal limit, and of course reading the
--   message.
--
-- The snippet below prints the names of each person in the address book:
--
-- > {-# LANGUAGE ScopedTypeVariables #-}
-- > import Prelude hiding (length)
-- >
-- > import Capnp.Gen.Addressbook
-- > import Capnp
-- >     (ConstMsg, defaultLimit, evalLimitT, getValue, index, length, textBytes)
-- >
-- > import           Control.Monad         (forM_)
-- > import           Control.Monad.Trans   (lift)
-- > import qualified Data.ByteString.Char8 as BS8
-- >
-- > main = do
-- >     addressbook :: AddressBook ConstMsg <- getValue defaultLimit
-- >     evalLimitT defaultLimit $ do
-- >         people <- get_AddressBook'people addressbook
-- >         forM_ [0..length people - 1] $ \i -> do
-- >             name <- index i people >>= get_Person'name >>= textBytes
-- >             lift $ BS8.putStrLn name
--
-- Note that we use the same `getValue` function as in the high-level
-- example above.

-- $lowlevel-write
--
-- Writing messages using the low-level API has a similarly imperative feel.
-- The below constructs the same message as in our high-level example above:
--
-- > import Capnp.Gen.Addressbook
-- >
-- > import Capnp
-- >     ( MutMsg
-- >     , PureBuilder
-- >     , cerialize
-- >     , createPure
-- >     , defaultLimit
-- >     , index
-- >     , newMessage
-- >     , newRoot
-- >     , putMsg
-- >     )
-- >
-- > import qualified Data.Text as T
-- >
-- > main =
-- >     let Right msg = createPure defaultLimit buildMsg
-- >     in putMsg msg
-- >
-- > buildMsg :: PureBuilder s (MutMsg s)
-- > buildMsg = do
-- >     -- newMessage allocates a new, initially empty, mutable message:
-- >     msg <- newMessage
-- >
-- >     -- newRoot allocates a new struct as the root object of the message.
-- >     -- In this case the type of the struct can be inferred from our later
-- >     -- use of AddressBook's accessors:
-- >     addressbook <- newRoot msg
-- >
-- >     -- new_* accessors allocate a new value of the correct type for a
-- >     -- given field. These functions accordingly only exist for types
-- >     -- which are encoded as pointers (structs, lists, bytes...). In
-- >     -- the case of lists, these take an extra argument specifying a
-- >     -- the length of the list:
-- >     people <- new_AddressBook'people 2 addressbook
-- >
-- >     -- Index gets an object at a specified location in a list. Cap'N Proto
-- >     -- lists are flat arrays, and in the case of structs the structs are
-- >     -- unboxed, so there is no need to allocate each element:
-- >     alice <- index 0 people
-- >
-- >     -- set_* functions set the value of a field. For fields of non-pointer
-- >     -- types (integers, bools...), We can just pass the value we want to set_*,
-- >     -- rather than allocating via new_* first:
-- >     set_Person'id alice 123
-- >
-- >     -- 'cerialize' is used to marshal a value into a message. Below, we copy
-- >     -- the text for Alice's name and email address into the message, and then
-- >     -- use Person's set_* functions to attach the resulting objects to our
-- >     -- Person:
-- >     set_Person'name alice =<< cerialize msg (T.pack "Alice")
-- >     set_Person'email alice =<< cerialize msg (T.pack "alice@example.com")
-- >
-- >     phones <- new_Person'phones 1 alice
-- >     mobilePhone <- index 0 phones
-- >     set_Person'PhoneNumber'number mobilePhone =<< cerialize msg (T.pack "555-1212")
-- >     set_Person'PhoneNumber'type_ mobilePhone Person'PhoneNumber'Type'mobile
-- >
-- >     -- Setting union fields is slightly awkward; we have an auxiliary type
-- >     -- for the union field, which we must get_* first:
-- >     employment <- get_Person'employment alice
-- >
-- >     -- Then, we can use set_* to set both the tag of the union and the
-- >     -- value:
-- >     set_Person'employment'school employment =<< cerialize msg (T.pack "MIT")
-- >
-- >     bob <- index 1 people
-- >     set_Person'id bob 456
-- >     set_Person'name bob =<< cerialize msg (T.pack "Bob")
-- >     set_Person'email bob =<< cerialize msg (T.pack "bob@example.com")
-- >
-- >     phones <- new_Person'phones 2 bob
-- >     homePhone <- index 0 phones
-- >     set_Person'PhoneNumber'number homePhone =<< cerialize msg (T.pack "555-4567")
-- >     set_Person'PhoneNumber'type_ homePhone Person'PhoneNumber'Type'home
-- >     workPhone <- index 1 phones
-- >     set_Person'PhoneNumber'number workPhone =<< cerialize msg (T.pack "555-7654")
-- >     set_Person'PhoneNumber'type_ workPhone Person'PhoneNumber'Type'work
-- >     employment <- get_Person'employment bob
-- >     set_Person'employment'selfEmployed employment
-- >
-- >     pure msg

-- $rpc
--
-- This package supports level 1 Cap'n Proto RPC. The tuotrial will demonstrate the most
-- basic features of the RPC system with example: an echo server & client. For a larger
-- example which demos more of the protocol's capabilities, see the calculator example
-- in the source repository's @examples/@ directory.
--
-- Given the schema:
--
-- > @0xd0a87f36fa0182f5;
-- >
-- > interface Echo {
-- >   echo @0 (query :Text) -> (reply :Text);
-- > }
--
-- In the low level module, the code generator generates a newtype wrapper called @Echo@
-- around a capability.
--
-- Most of the interesting stuff is in the high-level module (but note that you can still
-- do RPC using low-level serialization APIs). The code generator will create an API like
-- (after a bit of cleanup):
--
-- > newtype Echo = Echo Client
-- >
-- > class MonadIO m => Echo'server_ m cap where
-- >     echo'echo :: cap -> Server.MethodHandler m Echo'echo'params Echo'echo'results
-- >
-- > instance Echo'server_ IO Echo
-- >
-- > export_Echo :: Echo'server_ IO a => Supervisors -> a -> STM Echo
--
-- The type @Echo@ is a handle to an object (possibly remote), which can be used to
-- make method calls. It is a newtype wrapper around a 'Client', which provides
-- similar facilities, but doesn't know about the schema.
--
-- To provide an implementation of the @Echo@ interface, you need an instance of the
-- @Echo'server_@ type class. The @export_Echo@ function is used to convert such an
-- instance into a handle to the object that can be passed around.
--
-- Each time you call @export_Function@, it creates a thread that handles incoming
-- messages in sequence.
--
-- Note that capnproto does not have a notion of "clients" and "servers" in the
-- traditional networking sense; the two sides of a connection are symmetric. In
-- capnproto terminology, a "client" is a handle for calling methods, and a "server"
-- is an object that handles methods -- but there may be many of either or both of
-- these on each side of a connection.
--
-- Here is an an echo (networking) server using this interface:
--
-- > {-# LANGUAGE MultiParamTypeClasses #-}
-- > {-# LANGUAGE OverloadedStrings     #-}
-- > import Network.Simple.TCP (serve)
-- >
-- > import Capnp     (def, defaultLimit)
-- > -- 'Capnp.Rpc' exposes the most commonly used parts of the RPC system:
-- > import Capnp.Rpc
-- >     (ConnConfig(..), handleConn, pureHandler, socketTransport, toClient)
-- >
-- > import Capnp.Gen.Echo.Pure
-- >
-- > -- | A type to declare an instance on:
-- > data MyEchoServer = MyEchoServer
-- >
-- > -- The main logic of an echo server:
-- > instance Echo'server_ IO MyEchoServer where
-- >     -- Each method of an interface generates a corresponding
-- >     -- method in its type class. The name of the method is prefixed
-- >     -- with the name of the interface, so method bar on interface
-- >     -- Foo will be called foo'bar.
-- >     --
-- >     -- The type of a method is left abstract, and functions like
-- >     -- 'pureHandler' are used to construct method handlers; see the
-- >     -- "Handling method calls" section in the docs for 'Capnp.Rpc'.
-- >     echo'echo = pureHandler $ \MyEchoServer params ->
-- >         pure def { reply = query params }
-- >
-- > main :: IO ()
-- > main = serve "localhost" "4000" $ \(sock, _addr) ->
-- >     -- once we get a network connection, we use 'handleConn' to start
-- >     -- the rpc subsystem on that connection. It takes a transport with
-- >     -- which to send messages, and a config.
-- >     handleConn (socketTransport sock defaultLimit) def
-- >         { getBootstrap = \sup ->
-- >            -- The only setting we override in this example is our
-- >            -- bootstrap interface. The bootstrap interface is a "default"
-- >            -- object that clients can request on startup. By default
-- >            -- there is none, here we provide a client for our echo server.
-- >            Just . toClient <$> export_Echo sup MyEchoServer
-- >         }
--
-- The echo client looks like:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Examples.Rpc.EchoClient (main) where
-- >
-- > import Network.Simple.TCP (connect)
-- >
-- > import Capnp     (def, defaultLimit)
-- > import Capnp.Rpc (ConnConfig(..), handleConn, socketTransport, wait, (?))
-- >
-- > import Capnp.Gen.Echo.Pure
-- >
-- > main :: IO ()
-- > main = connect "localhost" "4000" $ \(sock, _addr) ->
-- >     handleConn (socketTransport sock defaultLimit) def
-- >         -- In this case, we leave 'getBootstrap' empty and set
-- >         -- 'withBootstrap', which will request the other side's
-- >         -- bootstrap interface. If a non-Nothing value is supplied for
-- >         -- 'withBootstrap', 'handleConn' will exit (and disconnect)
-- >         -- when it completes.
-- >         { withBootstrap = Just $ \_sup client ->
-- >             -- Clients also have instances of their server_ classes, so
-- >             -- can use these instances to call methods on the remote
-- >             -- object. The '?' is the message send operator.
-- >             --
-- >             -- The method call _immediately_ returns, yielding a promise
-- >             -- that will be fulfilled when the results of the call actually
-- >             -- arive. We use 'wait' to wait for the promise to resolve,
-- >             -- display the result to the user, and then exit.
-- >             echo'echo (Echo client) ? def { query = "Hello, World!" }
-- >                 >>= wait
-- >                 >>= print
-- >         }
