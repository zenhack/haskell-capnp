module Tests.SchemaQuickCheck
    (schemaCGRQuickCheck)
    where

import qualified Data.ByteString as BS

import Data.Capnp.Errors         (Error)
import Data.Capnp.Message        as M
import Data.Capnp.TraversalLimit (LimitT, runLimitT)

import qualified Capnp.ById.Xa93fc509624c72d9 as Schema
import qualified Data.Capnp.Untyped           as Untyped

-- Testing framework imports
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

-- Schema generation imports
import Tests.SchemaGeneration
import Tests.Util

-- Schema validation imports
import Control.Monad.Catch as C

-- Functions to generate valid CGRs

generateCGR :: Schema -> IO BS.ByteString
generateCGR schema = capnpCompile (show schema) "-"

-- Functions to validate CGRs

decodeCGR :: BS.ByteString -> IO (Int, Int)
decodeCGR bytes = do
    let reader :: Untyped.Struct (LimitT IO) BS.ByteString -> LimitT IO Int
        reader struct = do
            let req = Schema.CodeGeneratorRequest struct
            nodes <- Schema.get_CodeGeneratorRequest'nodes req
            requestedFiles <- Schema.get_CodeGeneratorRequest'requestedFiles req
            return (Untyped.length nodes)
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
