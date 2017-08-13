{-# LANGUAGE RecordWildCards, TypeFamilies #-}
module Tests.Control.Monad.CapNProto.MessageBuilder (buildTests) where

import Control.Monad.CapNProto.MessageBuilder
import Data.CapNProto.Bits (ByteCount(..), wordsToBytes)
import Data.CapNProto.Blob (BlobSlice(..))
import Data.CapNProto.Schema (Field(..))

import Control.Monad (void)
import Control.Monad.Primitive (RealWorld)
import Data.Primitive.ByteArray (readByteArray)
import Data.Int
import Data.Word

import Tests.Util (assertionsToTest)

import Test.HUnit (assertEqual, Assertion)


buildTest :: BuilderT p RealWorld IO () -> [Word8] -> Assertion
buildTest builder expected = do
    (BlobSlice{..}, ()) <- runBuilderT builder
    let ByteCount off = wordsToBytes offset
    let ByteCount len = wordsToBytes sliceLen
    actual <- mapM (readByteArray blob) [off..off+len-1]
    assertEqual "" expected actual


buildTests = assertionsToTest "build tests" $ map (uncurry buildTest)
    [ ( return ()
      , []
      )
    , ( void $ alloc 1
      , [0, 0, 0, 0, 0, 0, 0, 0]
      )
    , ( void $ withParent 1 $ buildSelf (256 :: Word64) 0 0
      , [0, 1, 0, 0, 0, 0, 0, 0]
      )
    , ( void $ withParent 1 $ do
            buildSelf (     7     ::  Word8) 0 0
            buildSelf (0xe0f0     :: Word16) 0 16
            buildSelf (0x01020304 :: Word32) 0 32
      , [7, 0, 0xf0, 0xe0, 0x04, 0x03, 0x02, 0x01]
      )
    , ( void $ withParent 1 $ do
            buildSelf (0xf0 :: Word8) 0 0
            buildSelf True 0 1
      , [0xf2, 0, 0, 0, 0, 0, 0, 0]
      )
    , ( void $ withParent 1 $ do
            buildSelf ((-1) ::  Int16) 0 16
      , [0, 0, 0xff, 0xff, 0, 0, 0, 0]
      )
    , ( void $ withParent 2 $ DataField 1 8  %~ (7 :: Word8)
      , [0, 0, 0, 0, 0, 0, 0, 0
        ,0, 7, 0, 0, 0, 0, 0, 0
        ]
      )
    , ( void $ withParent 1 $ DataField 0 33 %~ True
      , [0, 0, 0, 0, 2, 0, 0, 0]
      )
    , (void $ withParent 1 $ GroupField <~ do
            DataField 0 0  %~ (0xffff :: Word16)
            DataField 0 17 %~ True
      , [0xff, 0xff, 2, 0, 0, 0, 0, 0]
      )
    ]
