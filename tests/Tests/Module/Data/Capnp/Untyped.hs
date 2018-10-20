{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Tests.Module.Data.Capnp.Untyped (untypedTests) where

import Prelude hiding (length)

import Control.Monad                        (forM_, when)
import Control.Monad.Primitive              (RealWorld)
import Data.ReinterpretCast                 (doubleToWord, wordToDouble)
import Data.Text                            (Text)
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit                           (assertEqual)
import Test.QuickCheck                      (Property)
import Test.QuickCheck.IO                   (propertyIO)
import Text.Heredoc                         (here, there)

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

import Capnp.Untyped
import Tests.Util

import Capnp                (cerialize, createPure, def, getRoot, newRoot)
import Capnp.TraversalLimit (LimitT, evalLimitT, execLimitT)
import Data.Mutable         (Thaw(..))

import Instances ()

import Capnp.Gen.Capnp.Schema.Pure (Brand, Method(..), Node'Parameter)

import qualified Capnp.Classes as C
import qualified Capnp.Message as M

import qualified Capnp.Gen.Capnp.Schema as Schema

untypedTests = testGroup "Untyped Tests"
    [ readTests
    , modifyTests
    , farPtrTest
    , otherMessageTest
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

data ModTest s = ModTest
    { testIn   :: String
    , testMod  :: Struct (M.MutMsg RealWorld) -> LimitT IO ()
    , testOut  :: String
    , testType :: String
    }

modifyTests :: Test
modifyTests = testGroup "Test modification" $ map testCase
    -- tests for setIndex
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
    -- tests for allocation functions
    , ModTest
        { testIn = "()"
        , testType = "StackingRoot"
        , testOut = "( aWithDefault = (num = 6400),\n  a = (num = 65, b = (num = 90000)) )\n"

        , testMod = \struct -> do
            when (length (ptrSection struct) /= 2) $
                error "struct's pointer section is unexpedly small"

            let msg = message struct
            a <- allocStruct msg 1 1
            aWithDefault <- allocStruct msg 1 1
            b <- allocStruct msg 1 0
            setPtr (Just (PtrStruct b)) 0 a
            setPtr (Just (PtrStruct aWithDefault)) 0 struct
            setPtr (Just (PtrStruct a)) 1 struct
            setData 65 0 a
            setData 6400 0 aWithDefault
            setData 90000 0 b
        }
    , ModTest
        { testIn = "()"
        , testType = "HoldsVerTwoTwoList"
        , testOut = "( mylist = [(val = 0, duo = 70), (val = 0, duo = 71), (val = 0, duo = 72), (val = 0, duo = 73)] )\n"
        , testMod = \struct -> do
            mylist <- allocCompositeList (message struct) 2 2 4
            forM_ [0..3] $ \i ->
                index i mylist >>= setData (70 + fromIntegral i) 1
            setPtr (Just $ PtrList $ ListStruct mylist) 0 struct
        }
    , allocNormalListTest "u64vec" 21 allocList64 List64
    , allocNormalListTest "u32vec" 22 allocList32 List32
    , allocNormalListTest "u16vec" 23 allocList16 List16
    , allocNormalListTest "u8vec"  24 allocList8  List8
    , ModTest
        { testIn = "()"
        , testType = "Z"
        , testOut = "( boolvec = [true, false, true] )\n"
        , testMod = \struct -> do
            setData 39 0 struct -- Set the union tag.
            boolvec <- allocList1 (message struct) 3
            forM_ [0..2] $ \i ->
                setIndex (even i) i boolvec
            setPtr (Just $ PtrList $ List1 boolvec) 0 struct
        }
    ]
  where
    -- generate a ModTest for a (normal) list allocation function.
    --
    -- parameters:
    --
    -- * tagname   - the name of the union variant
    -- * tagvalue  - the numeric value of the tag for this variant
    -- * allocList - the allocation function
    -- * dataCon   - the data constructor for 'List' to use.
    allocNormalListTest tagname tagvalue allocList dataCon =
        ModTest
            { testIn = "()"
            , testType = "Z"
            , testOut = "(" ++ tagname ++ " = [0, 1, 2, 3, 4])\n"
            , testMod = \struct -> do
                setData tagvalue 0 struct
                vec <- allocList (message struct) 5
                forM_ [0..4] $ \i -> setIndex (fromIntegral i) i vec
                setPtr (Just $ PtrList $ dataCon vec) 0 struct
            }
    testCase ModTest{..} = assertionsToTest
            (show testIn ++ " : " ++ testType ++ " == " ++ show testOut) $
            pure $ do
        msg <- thaw =<< encodeValue schemaText testType testIn
        evalLimitT 128 $ rootPtr msg >>= testMod
        actualOut <- decodeValue schemaText testType =<< freeze msg
        assertEqual ( actualOut ++ " == " ++ testOut) actualOut testOut
    schemaText = [there|tests/data/aircraft.capnp|]


farPtrTest = assertionsToTest
    "Setting cross-segment pointers should work."
    [ do
        msg <- M.newMessage
        -- The allocator always allocates new objects in the last segment, so
        -- if we create a new segment, the call to allocStruct below should
        -- allocate there:
        (1, _) <- M.newSegment msg 16
        struct <- allocStruct msg 3 4
        setRoot struct
    , evalLimitT maxBound $ do
        msg <- M.newMessage
        srcStruct <- allocStruct msg 4 4
        (1, _) <- M.newSegment msg 10
        dstStruct <- allocStruct msg 2 2
        ptr <- C.toPtr msg dstStruct
        setPtr ptr 0 srcStruct
    ]

otherMessageTest :: Test
otherMessageTest = testProperty
    "Setting pointers to values in other messages copies them if needed."
    otherMessageTest'

otherMessageTest' :: Text -> V.Vector Node'Parameter -> Brand -> Property
otherMessageTest' name params brand = propertyIO $ do
    let expected = def
            { name = name
            , implicitParameters = params
            , paramBrand = brand
            }
    msg :: M.ConstMsg <- createPure maxBound $ do
            methodMsg <- M.newMessage
            nameMsg <- M.newMessage
            paramsMsg <- M.newMessage
            brandMsg <- M.newMessage

            methodCerial <- newRoot methodMsg
            nameCerial <- cerialize nameMsg name
            brandCerial <- cerialize brandMsg brand

            -- We don't implement Cerialize for Vector, so we can't just
            -- inject params directly. TODO: implement Cerialize for Vector.
            wrapper <- cerialize paramsMsg expected
            paramsCerial <- Schema.get_Method'implicitParameters wrapper

            Schema.set_Method'name methodCerial nameCerial
            Schema.set_Method'implicitParameters methodCerial paramsCerial
            Schema.set_Method'paramBrand methodCerial brandCerial

            pure methodMsg
    actual <- evalLimitT maxBound $ getRoot msg >>= C.decerialize
    assertEqual (show actual ++ " == " ++ show expected) actual expected
