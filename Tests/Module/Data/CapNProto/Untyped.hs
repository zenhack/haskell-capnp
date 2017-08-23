{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Tests.Module.Data.CapNProto.Untyped where


import qualified Data.ByteString as BS
import Data.ReinterpretCast (wordToDouble)
import Text.Heredoc (here, there)
import Prelude hiding (length)

import Control.Monad (forM_, when)

import Test.HUnit (assertEqual)

import Tests.Util
import Control.Monad.Quota
import Data.CapNProto.Untyped
import qualified Data.CapNProto.Message as M

aircraftSchema = [there|testdata/aircraft.capnp|]

untypedTests = assertionsToTest "Untyped Tests"  $ map tst
    [ ( aircraftSchema
      , "Aircraft"
      , [here|(f16 = (base = (
            name = "bob",
            homes = [],
            rating = 7,
            canFly = true,
            capacity = 5173,
            maxSpeed = 12.0,
        )))|]
      , 128
      , \(Just (PtrStruct root)) -> do
            s <- get root
            aircraftWords <- dataSection s
            -- Aircraft just has the union tag, nothing else in it's data
            -- section.
            1 <- length aircraftWords
            3 <- get =<< index 0 aircraftWords -- tag for F16
            aircraftPtrSec <- ptrSection s
            1 <- length aircraftPtrSec
            Just (PtrStruct f16Ptr) <- get =<< index 0 aircraftPtrSec
            f16 <- get f16Ptr
            0 <- length =<< dataSection f16
            f16PtrSec <- ptrSection f16
            1 <- length f16PtrSec
            Just (PtrStruct basePtr) <- get =<< index 0 f16PtrSec
            base <- get basePtr
            baseWords <- dataSection base
            basePtrSec <- ptrSection base
            4 <- length baseWords -- Except canFly, each field is 1 word, and
                                  -- canFly is aligned such that it ends up
                                  -- consuming a whole word.
            2 <- length basePtrSec -- name, homes

            -- Walk the data section:
            7 <- get =<< index 0 baseWords -- rating
            1 <- get =<< index 1 baseWords -- canFly
            5173 <- get =<< index 2 baseWords -- capacity
            12.0 <- wordToDouble <$> (get =<< index 3 baseWords)

            -- ...and the pointer section:
            Just (PtrList namePtr) <- get =<< index 0 basePtrSec

            List8 name <- get namePtr
            -- Text values have a NUL terminator, which is included in the
            -- length on the wire. The spec says that this shouldn't be
            -- included in the length reported to the caller, but that needs
            -- to be dealt with by schema-aware code, so this is the length of
            -- "bob\0"
            4 <- length name

            forM_ (zip [0..3] (BS.unpack "bob\0")) $ \(i, c) -> do
                c' <- get =<< index i name
                when (c /= c') $
                    error ("index " ++ show i ++ ": " ++ show c ++ " /= " ++ show c')
            Just (PtrList homesPtr) <- get =<< index 1 basePtrSec
            List16 homes <- get homesPtr
            0 <- length homes
            return ()
      , ((), Quota 96)
      )
    ]
  where
    tst (schema, typename, value, quota, m, expected) = do
        let meta = MsgMetaData schema typename
        msg <- capnpEncode value meta >>= M.decode
        actual <- runQuotaT (rootPtr msg >>= m) (Quota quota)
        assertEqual (show (meta, value)) expected actual
