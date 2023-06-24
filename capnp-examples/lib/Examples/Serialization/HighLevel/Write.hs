-- Note that DuplicateRecordFields is usually needed, as the generated
-- code relys on it to resolve collisions in capnproto struct field
-- names:
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.Serialization.HighLevel.Write (main) where

-- Note that Capnp re-exports `def`, as a convienence
import Capnp (def, putParsed)
import Capnp.Gen.Addressbook

main :: IO ()
main =
  putParsed
    AddressBook
      { people =
          [ Person
              { id = 123,
                name = "Alice",
                email = "alice@example.com",
                phones =
                  [ def
                      { number = "555-1212",
                        type_ = Person'PhoneNumber'Type'mobile
                      }
                  ],
                employment = Person'employment' $ Person'employment'school "MIT"
              },
            Person
              { id = 456,
                name = "Bob",
                email = "bob@example.com",
                phones =
                  [ def
                      { number = "555-4567",
                        type_ = Person'PhoneNumber'Type'home
                      },
                    def
                      { number = "555-7654",
                        type_ = Person'PhoneNumber'Type'work
                      }
                  ],
                employment = Person'employment' Person'employment'selfEmployed
              }
          ]
      }
