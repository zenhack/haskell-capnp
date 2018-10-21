{-# LANGUAGE ScopedTypeVariables #-}
module Tests.SchemaQuickCheck
    (schemaCGRQuickCheck)
    where

import qualified Data.ByteString as BS

import Capnp.Classes        (fromStruct)
import Capnp.Errors         (Error)
import Capnp.Message        as M
import Capnp.TraversalLimit (LimitT, runLimitT)

import qualified Capnp.Basics           as Basics
import qualified Capnp.Gen.Capnp.Schema as Schema
import qualified Capnp.Untyped          as Untyped

-- Testing framework imports
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

-- Schema generation imports
import Tests.SchemaGeneration
import Util

-- Schema validation imports
import Control.Monad.Catch as C

-- Functions to generate valid CGRs

generateCGR :: Schema -> IO BS.ByteString
generateCGR schema = capnpCompile (show schema) "-"

-- Functions to validate CGRs

decodeCGR :: BS.ByteString -> IO (Int, Int)
decodeCGR bytes = do
    let reader :: Untyped.Struct M.ConstMsg -> LimitT IO Int
        reader struct = do
            req :: Schema.CodeGeneratorRequest M.ConstMsg <- fromStruct struct
            nodes <- Schema.get_CodeGeneratorRequest'nodes req
            requestedFiles <- Schema.get_CodeGeneratorRequest'requestedFiles req
            return (Basics.length nodes)
    msg <- M.decode bytes
    (numNodes, endQuota) <- runLimitT 1024 (Untyped.rootPtr msg >>= reader)
    return (endQuota, numNodes)

-- QuickCheck properties

prop_schemaValid :: Schema -> Property
prop_schemaValid schema = ioProperty $ do
    compiled <- generateCGR schema
    decoded <- try $ decodeCGR compiled
    return $ case (decoded :: Either Error (Int, Int)) of
        Left _  -> False
        Right _ -> True

schemaCGRQuickCheck = testProperty "valid schema QuickCheck"
                      (prop_schemaValid <$> genSchema)
