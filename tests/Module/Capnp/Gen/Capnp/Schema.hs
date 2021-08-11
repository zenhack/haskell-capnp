{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module Module.Capnp.Gen.Capnp.Schema (schemaTests) where

import Test.Hspec

import Control.Monad.Primitive (RealWorld)
import Data.Foldable           (traverse_)
import Data.Function           ((&))

import qualified Capnp.Gen.Capnp.Schema.New as N

import Capnp.New            (encodeField, encodeVariant, initVariant, readField)
import Capnp.TraversalLimit (LimitT, evalLimitT)
import Data.Mutable         (Thaw(..))
import Util                 (decodeValue, schemaSchemaSrc)

import qualified Capnp.Message     as M
import qualified Capnp.New.Classes as NC

data BuildTest = BuildTest
    { typeName :: String
    , expected :: String
    , builder  :: M.Message ('M.Mut RealWorld) -> LimitT IO ()
    }

schemaTests :: Spec
schemaTests = describe "tests for typed setters" $ traverse_ testCase
    [ BuildTest
        { typeName = "Field"
        , expected = concat
            [ "( codeOrder = 4,\n"
            , "  discriminantValue = 6,\n"
            , "  group = (typeId = 322),\n"
            , "  ordinal = (explicit = 22) )\n"
            ]
        , builder = \msg -> do
            field <- NC.newRoot @N.Field () msg
            field & encodeField #codeOrder 4
            field & encodeField #discriminantValue 6
            field
                & initVariant #group
                >>= encodeField #typeId 322
            field
                & readField #ordinal
                >>= encodeVariant #explicit 22
        }
    ]
  where
    testCase BuildTest{..} = it ("Should build " ++ expected) $ do
        msg <- M.newMessage Nothing
        evalLimitT maxBound $ builder msg
        constMsg <- freeze msg
        actual <- decodeValue schemaSchemaSrc typeName constMsg
        actual `shouldBe` expected
