{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Tests.Module.Data.Capnp.Untyped (untypedTests) where

import Prelude hiding (length)

import Data.Capnp.Untyped
import Tests.Util

import Control.Monad             (forM_, when)
import Data.Capnp.TraversalLimit (execWithLimit)
import Data.ReinterpretCast      (wordToDouble)
import Test.Framework            (Test)
import Test.HUnit                (assertEqual)
import Text.Heredoc              (here, there)

import qualified Data.ByteString as BS

untypedTests :: Test
untypedTests = assertionsToTest "Untyped Tests"
    [ do
        msg <- encodeValue
                    [there|tests/data/aircraft.capnp|]
                    "Aircraft"
                    [here|(f16 = (base = (
                       name = "bob",
                       homes = [],
                       rating = 7,
                       canFly = true,
                       capacity = 5173,
                       maxSpeed = 12.0
                    )))|]
        endQuota <- execWithLimit 128 $ do
            root <- rootPtr msg
            let aircraftWords = dataSection root
            -- Aircraft just has the union tag, nothing else in it's data
            -- section.
            let 1 = length aircraftWords
            3 <- index 0 aircraftWords -- tag for F16
            let 1 = length (ptrSection root)
            Just (PtrStruct f16) <- getPtr 0 root
            let 0 = length (dataSection f16)
            let 1 = length (ptrSection f16)
            Just (PtrStruct base) <- getPtr 0 f16
            let 4 = length (dataSection base) -- Except canFly, each field is 1 word, and
                                              -- canFly is aligned such that it ends up
                                              -- consuming a whole word.
            let 2 = length (ptrSection base) -- name, homes

            -- Walk the data section:
            7 <- getData 0 base -- rating
            1 <- getData 1 base -- canFly
            5173 <- getData 2 base -- capacity
            12.0 <- wordToDouble <$> getData 3 base

            -- ...and the pointer section:
            Just (PtrList (List8 name)) <- getPtr 0 base
            -- Text values have a NUL terminator, which is included in the
            -- length on the wire. The spec says that this shouldn't be
            -- included in the length reported to the caller, but that needs
            -- to be dealt with by schema-aware code, so this is the length of
            -- "bob\0"
            let 4 = length name

            forM_ (zip [0..3] (BS.unpack "bob\0")) $ \(i, c) -> do
                c' <- index i name
                when (c /= c') $
                    error ("index " ++ show i ++ ": " ++ show c ++ " /= " ++ show c')
            Just (PtrList (List16 homes)) <- getPtr 1 base
            let 0 = length homes
            return ()
        assertEqual "endQuota == 110" 110 endQuota
    ]
