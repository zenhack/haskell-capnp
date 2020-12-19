{-# LANGUAGE DataKinds #-}
module Examples.Serialization.LowLevel.Write (main) where
import Capnp.Gen.Addressbook

import Capnp
    ( Message
    , Mutability(Mut)
    , PureBuilder
    , cerialize
    , createPure
    , defaultLimit
    , index
    , newMessage
    , newRoot
    , putMsg
    )

import qualified Data.Text as T

main :: IO ()
main =
    let Right msg = createPure defaultLimit buildMsg
    in putMsg msg

buildMsg :: PureBuilder s (Message ('Mut s))
buildMsg = do
    -- newMessage allocates a new, initially empty, mutable message. It
    -- takes an optional size hint:
    msg <- newMessage Nothing

    -- newRoot allocates a new struct as the root object of the message.
    -- In this case the type of the struct can be inferred from our later
    -- use of AddressBook's accessors:
    addressbook <- newRoot msg

    -- new_* accessors allocate a new value of the correct type for a
    -- given field. These functions accordingly only exist for types
    -- which are encoded as pointers (structs, lists, bytes...). In
    -- the case of lists, these take an extra argument specifying a
    -- the length of the list:
    people <- new_AddressBook'people 2 addressbook

    -- Index gets an object at a specified location in a list. Cap'N Proto
    -- lists are flat arrays, and in the case of structs the structs are
    -- unboxed, so there is no need to allocate each element:
    alice <- index 0 people

    -- set_* functions set the value of a field. For fields of non-pointer
    -- types (integers, bools...), We can just pass the value we want to set_*,
    -- rather than allocating via new_* first:
    set_Person'id alice 123

    -- 'cerialize' is used to marshal a value into a message. Below, we copy
    -- the text for Alice's name and email address into the message, and then
    -- use Person's set_* functions to attach the resulting objects to our
    -- Person:
    set_Person'name alice =<< cerialize msg (T.pack "Alice")
    set_Person'email alice =<< cerialize msg (T.pack "alice@example.com")

    phones <- new_Person'phones 1 alice
    mobilePhone <- index 0 phones
    set_Person'PhoneNumber'number mobilePhone =<< cerialize msg (T.pack "555-1212")
    set_Person'PhoneNumber'type_ mobilePhone Person'PhoneNumber'Type'mobile

    -- Setting union fields is slightly awkward; we have an auxiliary type
    -- for the union field, which we must get_* first:
    employment <- get_Person'employment alice

    -- Then, we can use set_* to set both the tag of the union and the
    -- value:
    set_Person'employment'school employment =<< cerialize msg (T.pack "MIT")

    bob <- index 1 people
    set_Person'id bob 456
    set_Person'name bob =<< cerialize msg (T.pack "Bob")
    set_Person'email bob =<< cerialize msg (T.pack "bob@example.com")

    phones <- new_Person'phones 2 bob
    homePhone <- index 0 phones
    set_Person'PhoneNumber'number homePhone =<< cerialize msg (T.pack "555-4567")
    set_Person'PhoneNumber'type_ homePhone Person'PhoneNumber'Type'home
    workPhone <- index 1 phones
    set_Person'PhoneNumber'number workPhone =<< cerialize msg (T.pack "555-7654")
    set_Person'PhoneNumber'type_ workPhone Person'PhoneNumber'Type'work
    employment <- get_Person'employment bob
    set_Person'employment'selfEmployed employment

    pure msg
