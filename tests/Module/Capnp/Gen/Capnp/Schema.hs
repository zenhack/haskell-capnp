{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module Module.Capnp.Gen.Capnp.Schema (schemaTests) where

import Test.Hspec

import Control.Monad.Primitive (RealWorld)
import Data.Foldable           (traverse_)

import           Capnp.Gen.Capnp.Schema
import qualified Capnp.Gen.Capnp.Schema.New as N

import Capnp                (newRoot)
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
            field <- newRoot msg
            set_Field'codeOrder field 4
            set_Field'discriminantValue field 6
            group <- set_Field'group field
            set_Field'group'typeId group 322
            ordinal <- get_Field'ordinal field
            set_Field'ordinal'explicit ordinal 22
        }
    , BuildTest -- same test, but with the repr API.
        { typeName = "Field"
        , expected = concat
            [ "( codeOrder = 4,\n"
            , "  discriminantValue = 6,\n"
            , "  group = (typeId = 322),\n"
            , "  ordinal = (explicit = 22) )\n"
            ]
        , builder = \msg -> do
            field <- NC.newRoot @N.Field () msg
            encodeField #codeOrder 4 field
            encodeField #discriminantValue 6 field
            group <- initVariant #group field
            encodeField #typeId 322 group
            ordinal <- readField #ordinal field
            encodeVariant #explicit 22 ordinal
        }
    ]
  where
    testCase BuildTest{..} = it ("Should build " ++ expected) $ do
        msg <- M.newMessage Nothing
        evalLimitT maxBound $ builder msg
        constMsg <- freeze msg
        actual <- decodeValue schemaSchemaSrc typeName constMsg
        actual `shouldBe` expected
