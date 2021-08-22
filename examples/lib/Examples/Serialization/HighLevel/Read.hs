{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications      #-}
module Examples.Serialization.HighLevel.Read (main) where

import Capnp.Gen.Addressbook.New

import Capnp.New (defaultLimit, getParsed)

main :: IO ()
main = do
    value <- getParsed @AddressBook defaultLimit
    print value
