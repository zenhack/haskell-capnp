{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
module Tests.Module.Data.Capnp.Untyped.Generic (genericUntypedTests) where

import Tests.Util

import Control.Monad             (when)
import Data.Capnp.TraversalLimit (evalLimitT)
import Test.Framework            (Test)
import Text.Heredoc              (there)

import qualified Data.Capnp.Message.Generic as GM
import qualified Data.Capnp.Message.Mutable as MM
import qualified Data.Capnp.Untyped.Generic as GU

data ModTest m s = ModTest
    { testIn  :: String
    , testMod :: GU.Struct (MM.Message s) -> m ()
    , testOut :: String
    }

genericUntypedTests :: Test
genericUntypedTests = assertionsToTest "Test setIndex" $ map testCase
    [ ModTest
        { testIn = "(year = 2018, month = 6, day = 20)\n"
        , testMod = GU.setIndex 0 0 . GU.dataSection
        , testOut = "(year = 0, month = 0, day = 0)\n"
        }
    ]
  where
    testCase ModTest{..} = do
        msg <- GM.thaw =<< encodeValue schemaText "Zdate" testIn
        evalLimitT 128 $ GU.rootPtr msg >>= testMod
        actualOut <- decodeValue schemaText "Zdate" =<< GM.freeze msg
        when (actualOut /= testOut) $
            error $ "Expected:\n\n" ++ show testOut ++ "\n\n...but got:\n\n" ++ show actualOut
    schemaText = [there|tests/data/aircraft.capnp|]
