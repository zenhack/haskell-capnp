{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
module Examples.Serialization.LowLevel.Write (main) where

import Data.Function ((&))

import Capnp.Gen.Addressbook.New

import qualified Capnp.New as C
import qualified Data.Text as T

main :: IO ()
main =
    let Right msg = C.createPure C.defaultLimit buildMsg
    in C.putMsg msg

buildMsg :: C.PureBuilder s (C.Message ('C.Mut s))
buildMsg = do
    -- newMessage allocates a new, initially empty, mutable message. It
    -- takes an optional size hint:
    msg <- C.newMessage Nothing

    -- newRoot allocates a new struct as the root object of the message.
    -- The unit argument is a hint to the allocator to determine the size
    -- of the object; for types whose size is not fixed (e.g. untyped structs,
    -- lists), this may be something more meaningful.
    addressbook <- C.newRoot @AddressBook () msg

    -- newField can be used to allocate the value of a field, for pointer
    -- types like lists. The number is the allocation hint, as used by newRoot.
    -- We can use the OverloadedLabels extension to pass in fields by name.
    people <- C.newField #people 2 addressbook

    -- Index gets an object at a specified location in a list. Cap'N Proto
    -- lists are flat arrays, and in the case of structs the structs are
    -- unboxed, so there is no need to allocate each element:
    alice <- C.index 0 people

    -- encodeField takes the parsed form of a value and marshals it into
    -- the specified field. For basic types like integers & booleans, this
    -- is almost always what you want. For larger values, you may want to
    -- use newField as above, or separately create the value and use setField,
    -- as shown below.
    C.encodeField #id 123 alice
    C.encodeField #name (T.pack "Alice") alice
    C.encodeField #email (T.pack "alice@example.com") alice

    -- We would probably use newField here, but to demonstrate, we can allocate
    -- the value separately with new, and then set it with setField.
    phones <- C.new @(C.List Person'PhoneNumber) 1 msg
    C.setField #phones phones alice

    mobilePhone <- C.index 0 phones
    -- It is sometimes more ergonomic to use (&) from Data.Function. You might
    -- ask why not just make the container the first argument, but it works
    -- out better this way for the RPC examples.
    mobilePhone & C.encodeField #number (T.pack "555-1212")
    mobilePhone & C.encodeField #type_ Person'PhoneNumber'Type'mobile

    -- Since named unions act like unnamed unions inside a group, we first have
    -- to get the group field:
    employment <- C.readField #employment alice

    -- Then, we can use encodeVariant to set both the tag of the union and the
    -- value:
    employment & C.encodeVariant #school (T.pack "MIT")

    bob <- C.index 1 people
    bob & C.encodeField #id 456
    bob & C.encodeField #name (T.pack "Bob")
    bob & C.encodeField #email (T.pack "bob@example.com")

    phones <- bob & C.newField #phones 2
    homePhone <- phones & C.index 0
    homePhone & C.encodeField #number (T.pack "555-4567")
    homePhone & C.encodeField #type_ Person'PhoneNumber'Type'home
    workPhone <- phones & C.index 1
    workPhone & C.encodeField #number (T.pack "555-7654")
    workPhone & C.encodeField #type_ Person'PhoneNumber'Type'work
    employment <- bob & C.readField #employment
    employment & C.encodeVariant #selfEmployed () -- Note the (), since selfEmploy is Void.

    pure msg
