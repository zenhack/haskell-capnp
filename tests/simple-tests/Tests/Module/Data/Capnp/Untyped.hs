{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Tests.Module.Data.Capnp.Untyped (untypedTests) where

import Prelude hiding (length)

import Data.Capnp.Untyped
import Tests.Util

import Control.Monad             (forM_, when)
import Data.Capnp.TraversalLimit (evalLimitT, execLimitT)
import Data.ReinterpretCast      (doubleToWord, wordToDouble)
import Test.Framework            (Test, testGroup)
import Test.HUnit                (assertEqual)
import Text.Heredoc              (here, there)

import qualified Data.ByteString    as BS
import qualified Data.Capnp.Message as M

untypedTests = testGroup "Untyped Tests"
    [ readTests
    , setIndexTests
    ]

readTests :: Test
readTests = assertionsToTest "read tests"
    [ do
        msg <- encodeValue
                    [there|tests/data/aircraft.capnp|]
                    "Aircraft"
                    [here|(f16 = (base = (
                       name = "bob",
                       homes = [],
                       rating = 7,
                       canFly = true,
                       capacity = 5173,
                       maxSpeed = 12.0
                    )))|]
        endQuota <- execLimitT 128 $ do
            root <- rootPtr msg
            let aircraftWords = dataSection root
            -- Aircraft just has the union tag, nothing else in it's data
            -- section.
            let 1 = length aircraftWords
            3 <- index 0 aircraftWords -- tag for F16
            let 1 = length (ptrSection root)
            Just (PtrStruct f16) <- getPtr 0 root
            let 0 = length (dataSection f16)
            let 1 = length (ptrSection f16)
            Just (PtrStruct base) <- getPtr 0 f16
            let 4 = length (dataSection base) -- Except canFly, each field is 1 word, and
                                              -- canFly is aligned such that it ends up
                                              -- consuming a whole word.
            let 2 = length (ptrSection base) -- name, homes

            -- Walk the data section:
            7 <- getData 0 base -- rating
            1 <- getData 1 base -- canFly
            5173 <- getData 2 base -- capacity
            12.0 <- wordToDouble <$> getData 3 base

            -- ...and the pointer section:
            Just (PtrList (List8 name)) <- getPtr 0 base
            -- Text values have a NUL terminator, which is included in the
            -- length on the wire. The spec says that this shouldn't be
            -- included in the length reported to the caller, but that needs
            -- to be dealt with by schema-aware code, so this is the length of
            -- "bob\0"
            let 4 = length name

            forM_ (zip [0..3] (BS.unpack "bob\0")) $ \(i, c) -> do
                c' <- index i name
                when (c /= c') $
                    error ("index " ++ show i ++ ": " ++ show c ++ " /= " ++ show c')
            Just (PtrList (List16 homes)) <- getPtr 1 base
            let 0 = length homes
            return ()
        assertEqual "endQuota == 110" 110 endQuota
    ]

data ModTest m s = ModTest
    { testIn   :: String
    , testMod  :: Struct (M.MutMsg s) -> m ()
    , testOut  :: String
    , testType :: String
    }

setIndexTests :: Test
setIndexTests = assertionsToTest "Test setIndex" $ map testCase
    [ ModTest
        { testIn = "(year = 2018, month = 6, day = 20)\n"
        , testType = "Zdate"
        , testOut = "(year = 0, month = 0, day = 0)\n"
        , testMod = setIndex 0 0 . dataSection
        }
    , ModTest
        { testIn = "(text = \"Hello, World!\")\n"
        , testType = "Z"
        , testOut = "(text = \"hEllo, world!\")\n"
        , testMod = \struct -> do
            Just (PtrList (List8 list)) <- index 0 (ptrSection struct)
            setIndex (fromIntegral (fromEnum 'h')) 0 list
            setIndex (fromIntegral (fromEnum 'E')) 1 list
            setIndex (fromIntegral (fromEnum 'w')) 7 list
        }
    , ModTest
        { testIn = "(boolvec = [true, true, false, true])\n"
        , testType = "Z"
        , testOut = "( boolvec = [false, true, true, false] )\n"
        , testMod = \struct -> do
            Just (PtrList (List1 list)) <- index 0 (ptrSection struct)
            setIndex False 0 list
            setIndex True 2 list
            setIndex False 3 list
        }
    , ModTest
        { testIn = "(f64 = 2.0)\n"
        , testType = "Z"
        , testOut = "(f64 = 7.2)\n"
        , testMod = setIndex (doubleToWord 7.2) 1 . dataSection
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
            Just (PtrList (ListPtr list)) <- index 1 (ptrSection struct)
            helloWorld <- index 0 (ptrSection struct)
            oranges <- index 1 list
            setIndex oranges 0 (ptrSection struct)
            setIndex helloWorld 1 list
        }
    , ModTest
        { testIn = unlines
            [ "( aircraftvec = ["
            , "    ( f16 = ("
            , "        base = ("
            , "          name = \"alice\","
            , "          homes = [],"
            , "          rating = 7,"
            , "          canFly = true,"
            , "          capacity = 4,"
            , "          maxSpeed = 100 ) ) ),"
            , "    ( b737 = ("
            , "        base = ("
            , "          name = \"bob\","
            , "          homes = [],"
            , "          rating = 2,"
            , "          canFly = false,"
            , "          capacity = 9,"
            , "          maxSpeed = 50 ) ) ) ] )"
            ]
        , testType = "Z"
        , testOut = unlines
            [ "( aircraftvec = ["
            , "    ( f16 = ("
            , "        base = ("
            , "          name = \"alice\","
            , "          homes = [],"
            , "          rating = 7,"
            , "          canFly = true,"
            , "          capacity = 4,"
            , "          maxSpeed = 100 ) ) ),"
            , "    ( f16 = ("
            , "        base = ("
            , "          name = \"alice\","
            , "          homes = [],"
            , "          rating = 7,"
            , "          canFly = true,"
            , "          capacity = 4,"
            , "          maxSpeed = 100 ) ) ) ] )"
            ]
        , testMod = \struct -> do
            Just (PtrList (ListStruct list)) <- getPtr 0 struct
            src <- index 0 list
            setIndex src 1 list
        }
    ]
  where
    testCase ModTest{..} = do
        msg <- M.thaw =<< encodeValue schemaText testType testIn
        evalLimitT 128 $ rootPtr msg >>= testMod
        actualOut <- decodeValue schemaText testType =<< M.freeze msg
        when (actualOut /= testOut) $
            error $ "Expected:\n\n" ++ show testOut ++ "\n\n...but got:\n\n" ++ show actualOut
    schemaText = [there|tests/data/aircraft.capnp|]
