{-# LANGUAGE OverloadedStrings     #-}
-- Note that DuplicateRecordFields is usually needed, as the generated
-- code relys on it to resolve collisions in capnproto struct field
-- names:
{-# LANGUAGE DuplicateRecordFields #-}
module Examples.Serialization.HighLevel.Write (main) where
import Capnp.Gen.Addressbook.Pure

-- Note that Capnp re-exports `def`, as a convienence
import Capnp (def, putValue)

import qualified Data.Vector as V

main :: IO ()
main = putValue AddressBook
    { people = V.fromList
        [ Person
            { id = 123
            , name = "Alice"
            , email = "alice@example.com"
            , phones = V.fromList
                [ def
                    { number = "555-1212"
                    , type_ =  Person'PhoneNumber'Type'mobile
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
                    { number = "555-7654"
                    , type_ = Person'PhoneNumber'Type'work
                    }
                ]
            , employment = Person'employment'selfEmployed
            }
        ]
    }
