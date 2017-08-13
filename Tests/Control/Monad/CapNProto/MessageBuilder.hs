{-# LANGUAGE RecordWildCards, TypeFamilies #-}
module Tests.Control.Monad.CapNProto.MessageBuilder (buildTests) where

import Control.Monad.CapNProto.MessageBuilder
import Data.CapNProto.Bits (WordCount(..))
import Data.CapNProto.Blob (BlobSlice(..))

import Control.Monad.Primitive (RealWorld)
import Data.Primitive.ByteArray (readByteArray)
import Data.Word

import Tests.Util (assertionsToTest)

import Test.HUnit (assertEqual, Assertion)


buildTest :: BuilderT p RealWorld IO () -> [Word8] -> Assertion
buildTest builder expected = do
    (BlobSlice{..}, ()) <- runBuilderT builder
    let WordCount off = offset
    let WordCount len = sliceLen
    actual <- mapM (readByteArray blob) [off..off+len-1]
    assertEqual "" expected actual


buildTests = assertionsToTest "build tests" $ map (uncurry buildTest)
    [ ( return ()
      , []
      )
    ]
