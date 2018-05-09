{-# LANGUAGE QuasiQuotes #-}
module Tests.EncodeDecodeUntyped
    (encodeDecodeUntypedTests)
  where

-- These tests verify Control.Monad.Capnp.MessageBuilder and
-- Data.Capnp.Untyped against eachother and against capnp
-- encode/decode. They are not schema-aware.

import Prelude hiding (length)

import Control.Monad.Capnp.MessageBuilder
import Data.Capnp.Schema
import Data.Capnp.Untyped
import Data.Word
import Tests.Util

import Control.Monad                 (void)
import Control.Monad.Primitive       (RealWorld)
import Data.ByteString               (ByteString)
import Data.Capnp.TraversalLimit (LimitT, runWithLimit)
import Test.Framework                (Test)
import Test.HUnit                    (Assertion, assertEqual)
import Text.Heredoc                  (here)

import qualified Data.Capnp.Message as M

-- Cross check capnp encode/decode, builder, and the readers. Verifies that:
--
-- * The reader doesn't choke on the output of capnp encode
-- * The reader doesn't choke on the output of the builder
-- * capnp decode on the output of the builder yields the message text.
encodeDecodeUntypedTest
    :: ( MsgMetaData -- schema and type name
       , String -- message in textual form
       , BuilderT p RealWorld IO () -- Builder for the message
       , Struct ByteString -> LimitT IO () -- reader.
       , Int -- Quota to run the reader with.
       , Int -- Remaining quota for the reader.
       )
    -> Assertion
encodeDecodeUntypedTest (meta, msgText, builder, reader, startQuota, endQuota) = do
    (blobSlice, ()) <- runBuilderT (frameSegment builder)
    bytes <- freezeAsByteString blobSlice
    actualText <- capnpDecode bytes meta
    assertEqual "Builder to text" msgText actualText
    checkReader bytes
    checkReader =<< capnpEncode msgText meta
  where
    checkReader bytes = do
        msg <- M.decode bytes
        readerResult <- runWithLimit startQuota (rootPtr msg >>= reader)
        assertEqual (show (meta, msgText)) ((), endQuota) readerResult


encodeDecodeUntypedTests :: Test
encodeDecodeUntypedTests =
    assertionsToTest "encode-decode-untyped tests" $ map encodeDecodeUntypedTest
    [ ( MsgMetaData
         { msgSchema = [here|@0xaa6fda3b444d262c;
                          struct A {
                             a @0 :UInt64;
                             b @1 :Bool;
                          }
                       |]
         , msgType = "A"
         }
      , "(a = 72, b = true)\n"
      , void $ withParent 1 $ buildStruct 2 0 $ do
            DataField 0 0 %~ (72 :: Word64)
            DataField 1 0 %~ ( 1 :: Word64)
      , \root -> do
            let dataWords = dataSection root
            let 2 = length dataWords
            72 <- index 0 dataWords
            1 <- index 1 dataWords
            let ptrs = ptrSection root
            let 0 = length ptrs
            return ()
      , 128
      , 125
      )
    ]
