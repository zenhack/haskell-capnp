{-# LANGUAGE RecordWildCards, TypeFamilies #-}
module Tests.Control.Monad.CapNProto.MessageBuilder (buildTests) where

import Control.Monad.CapNProto.MessageBuilder
import Data.CapNProto.Bits (WordCount(..))
import Data.CapNProto.Blob (BlobSlice(..))

import Control.Monad.Primitive (RealWorld)
import Data.Primitive.ByteArray (readByteArray)
import Data.Word

import Test.Framework (testGroup)
import Test.HUnit (assertEqual, Test(TestList, TestCase))
import Test.Framework.Providers.HUnit (hUnitTestToTests)


buildTest :: BuilderT p RealWorld IO () -> [Word8] -> Test
buildTest builder expected =  TestCase $ do
    (BlobSlice{..}, ()) <- runBuilderT builder
    let WordCount off = offset
    let WordCount len = sliceLen
    actual <- mapM (readByteArray blob) [off..off+len]
    assertEqual "" expected actual


buildTests = testGroup "build tests" $ hUnitTestToTests $ TestList $ map (uncurry buildTest)
    [ ( return ()
      , []
      )
    ]
