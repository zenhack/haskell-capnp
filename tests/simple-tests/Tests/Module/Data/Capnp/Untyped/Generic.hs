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
    { testIn   :: String
    , testMod  :: GU.Struct (MM.Message s) -> m ()
    , testOut  :: String
    , testType :: String
    }

genericUntypedTests :: Test
genericUntypedTests = assertionsToTest "Test setIndex" $ map testCase
    [ ModTest
        { testIn = "(year = 2018, month = 6, day = 20)\n"
        , testType = "Zdate"
        , testOut = "(year = 0, month = 0, day = 0)\n"
        , testMod = GU.setIndex 0 0 . GU.dataSection
        }
    , ModTest
        { testIn = "(text = \"Hello, World!\")\n"
        , testType = "Z"
        , testOut = "(text = \"hEllo, world!\")\n"
        , testMod = \struct -> do
            Just (GU.PtrList (GU.List8 list)) <- GU.index 0 (GU.ptrSection struct)
            GU.setIndex (fromIntegral (fromEnum 'h')) 0 list
            GU.setIndex (fromIntegral (fromEnum 'E')) 1 list
            GU.setIndex (fromIntegral (fromEnum 'w')) 7 list
            return ()
        }
    , ModTest
        { testIn = "(boolvec = [true, true, false, true])\n"
        , testType = "Z"
        , testOut = "( boolvec = [false, true, true, false] )\n"
        , testMod = \struct -> do
            Just (GU.PtrList (GU.List1 list)) <- GU.index 0 (GU.ptrSection struct)
            GU.setIndex False 0 list
            GU.setIndex True 2 list
            GU.setIndex False 3 list
            return ()
        }
    ]
  where
    testCase ModTest{..} = do
        msg <- GM.thaw =<< encodeValue schemaText testType testIn
        evalLimitT 128 $ GU.rootPtr msg >>= testMod
        actualOut <- decodeValue schemaText testType =<< GM.freeze msg
        when (actualOut /= testOut) $
            error $ "Expected:\n\n" ++ show testOut ++ "\n\n...but got:\n\n" ++ show actualOut
    schemaText = [there|tests/data/aircraft.capnp|]
