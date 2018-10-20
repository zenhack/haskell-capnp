{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
module Tests.Module.Capnp.Capnp.Schema (schemaTests) where

import Control.Monad           (when)
import Control.Monad.Primitive (RealWorld)
import Text.Heredoc            (there)

import Capnp.Gen.Capnp.Schema

import Data.Capnp                (newRoot)
import Data.Capnp.TraversalLimit (LimitT, evalLimitT)
import Data.Mutable              (Thaw(..))
import Tests.Util                (assertionsToTest, decodeValue)

import qualified Data.Capnp.Message as M

data BuildTest = BuildTest
    { typeName :: String
    , expected :: String
    , builder  :: M.MutMsg RealWorld -> LimitT IO ()
    }

schemaTests = assertionsToTest "Test typed setters" $ map testCase
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
            union <- get_Field'union' field
            group <- set_Field'group union
            set_Field'group'typeId group 322
            ordinal <- get_Field'ordinal field
            set_Field'ordinal'explicit ordinal 22
        }
    ]
  where
    testCase BuildTest{..} = do
        msg <- M.newMessage
        evalLimitT maxBound $ builder msg
        constMsg <- freeze msg
        actual <- decodeValue schemaSchema typeName constMsg
        when (actual /= expected) $
            error $ "Expected:\n\n" ++ show expected ++ "\n\n...but got:\n\n" ++ show actual

schemaSchema = [there|core-schema/capnp/schema.capnp|]
