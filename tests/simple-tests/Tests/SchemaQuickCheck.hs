module Tests.SchemaQuickCheck
    (schemaCGRQuickCheck)
    where

import qualified Data.ByteString as BS

import Data.CapNProto.Blob           (BlobSlice)
import Data.CapNProto.Errors         (Error)
import Data.CapNProto.Message        as M
import Data.CapNProto.TraversalLimit (LimitT, runWithLimit)

import Schema.CapNProto.Reader.Schema as Schema hiding (Field)

import qualified Data.CapNProto.Untyped                              as Untyped
import qualified Schema.CapNProto.Reader.Schema.CodeGeneratorRequest as CGReq

-- Testing framework imports
import Test.Framework                       (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

-- Schema generation imports
import Tests.SchemaGeneration
import Tests.Util

-- Schema validation imports
import Control.Monad.Catch as C

-- Functions to generate valid CGRs

generateCGR :: Schema -> IO BS.ByteString
generateCGR schema = do
    pOut <- capnpCompile (show schema) "-"
    return pOut

-- Functions to validate CGRs

decodeCGR :: BS.ByteString -> IO (Int, Int)
decodeCGR bytes = do
    let reader :: Untyped.Struct BS.ByteString -> LimitT IO Int
        reader struct = do
            let req = Schema.CodeGeneratorRequest struct
            nodes <- CGReq.nodes req
            requestedFiles <- CGReq.requestedFiles req
            return (Untyped.length nodes)
    msg <- M.decode bytes
    (numNodes, endQuota) <- runWithLimit 1024 (Untyped.rootPtr msg >>= reader)
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
