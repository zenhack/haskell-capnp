{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Examples.Serialization.LowLevel.Read (main) where

import Capnp.Gen.Addressbook
import qualified Capnp as C
import Control.Monad (forM_)
import Control.Monad.Trans (lift)
import Data.Function ((&))
import qualified Data.Text as T

main :: IO ()
main = do
  addressbook <- C.getRaw @AddressBook C.defaultLimit
  C.evalLimitT C.defaultLimit $ do
    people <- C.readField #people addressbook
    forM_ [0 .. C.length people - 1] $ \i -> do
      people
        & C.index i
        >>= C.parseField #name
        >>= lift . putStrLn . T.unpack
