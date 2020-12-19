{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Examples.Serialization.LowLevel.Read (main) where

import Prelude hiding (length)

import Capnp
    ( Mutability(Const)
    , defaultLimit
    , evalLimitT
    , getValue
    , index
    , length
    , textBytes
    )
import Capnp.Gen.Addressbook

import           Control.Monad         (forM_)
import           Control.Monad.Trans   (lift)
import qualified Data.ByteString.Char8 as BS8

main :: IO ()
main = do
    addressbook :: AddressBook 'Const <- getValue defaultLimit
    evalLimitT defaultLimit $ do
        people <- get_AddressBook'people addressbook
        forM_ [0..length people - 1] $ \i -> do
            name <- index i people >>= get_Person'name >>= textBytes
            lift $ BS8.putStrLn name
