{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
module Tests.Module.Data.Capnp.Untyped.Generic (genericUntypedTests) where

import Tests.Util

import Control.Monad             (when)
import Data.Capnp.TraversalLimit (evalLimitT)
import Data.ReinterpretCast      (doubleToWord)
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
        }
    , ModTest
        { testIn = "(f64 = 2.0)\n"
        , testType = "Z"
        , testOut = "(f64 = 7.2)\n"
        , testMod = GU.setIndex (doubleToWord 7.2) 1 . GU.dataSection
        }
    , ModTest
        { testIn = unlines
            [ "( size = 4,"
            , "  words = \"Hello, World!\","
            , "  wordlist = [\"apples\", \"oranges\"] )"
            ]
        , testType = "Counter"
        , testOut = unlines
            [ "( size = 4,"
            , "  words = \"oranges\","
            , "  wordlist = [\"apples\", \"Hello, World!\"] )"
            ]
        , testMod = \struct -> do
            Just (GU.PtrList (GU.ListPtr list)) <- GU.index 1 (GU.ptrSection struct)
            helloWorld <- GU.index 0 (GU.ptrSection struct)
            oranges <- GU.index 1 list
            GU.setIndex oranges 0 (GU.ptrSection struct)
            GU.setIndex helloWorld 1 list
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
