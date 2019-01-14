{-# LANGUAGE DuplicateRecordFields #-}
module Examples.Serialization.HighLevel.Read (main) where

import Capnp.Gen.Addressbook.Pure

import Capnp (defaultLimit, getValue)

main :: IO ()
main = do
    value <- getValue defaultLimit
    print (value :: AddressBook)
