{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Tests.Module.Data.CapNProto.Untyped (untypedTests) where

import Prelude hiding (length)

import Control.Monad.Quota
import Data.CapNProto.Untyped
import Tests.Util

import Control.Monad        (forM_, when)
import Data.ReinterpretCast (wordToDouble)
import Test.Framework       (Test)
import Test.HUnit           (assertEqual)
import Text.Heredoc         (here, there)

import qualified Data.ByteString        as BS
import qualified Data.CapNProto.Message as M

aircraftSchema :: String
aircraftSchema = [there|tests/data/aircraft.capnp|]

untypedTests :: Test
untypedTests = assertionsToTest "Untyped Tests"  $ map tst
    [ ( aircraftSchema
      , "Aircraft"
      , [here|(f16 = (base = (
           name = "bob",
           homes = [],
           rating = 7,
           canFly = true,
           capacity = 5173,
           maxSpeed = 12.0
        )))|]
      , 128
      , \root -> do
            aircraftWords <- dataSection root
            -- Aircraft just has the union tag, nothing else in it's data
            -- section.
            let 1 = length aircraftWords
            3 <- index 0 aircraftWords -- tag for F16
            aircraftPtrSec <- ptrSection root
            let 1 = length aircraftPtrSec
            Just (PtrStruct f16) <- index 0 aircraftPtrSec
            0 <- length <$> dataSection f16
            f16PtrSec <- ptrSection f16
            let 1 = length f16PtrSec
            Just (PtrStruct base) <- index 0 f16PtrSec
            baseWords <- dataSection base
            basePtrSec <- ptrSection base
            let 4 = length baseWords -- Except canFly, each field is 1 word, and
                                     -- canFly is aligned such that it ends up
                                     -- consuming a whole word.
            let 2 = length basePtrSec -- name, homes

            -- Walk the data section:
            7 <- index 0 baseWords -- rating
            1 <- index 1 baseWords -- canFly
            5173 <- index 2 baseWords -- capacity
            12.0 <- wordToDouble <$> index 3 baseWords

            -- ...and the pointer section:
            Just (PtrList (List8 name)) <- index 0 basePtrSec
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
            Just (PtrList (List16 homes)) <- index 1 basePtrSec
            let 0 = length homes
            return ()
      , ((), 110)
      )
    ]
  where
    tst (schema, typename, value, quota, m, expected) = do
        let meta = MsgMetaData schema typename
        msg <- capnpEncode value meta >>= M.decode
        actual <- runQuotaT (rootPtr msg >>= m) quota
        assertEqual (show (meta, value)) expected actual
