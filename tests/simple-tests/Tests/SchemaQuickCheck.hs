module Tests.SchemaQuickCheck
    (schemaCGRQuickCheck)
  where

import qualified Data.ByteString as BS

import Prelude hiding (length)
import Data.CapNProto.Untyped (length, Ptr(PtrStruct), rootPtr)
import Data.CapNProto.Blob (BlobSlice)
import Data.CapNProto.Message as M
import Schema.CapNProto.Reader.Schema as Schema hiding (Field)
import qualified Schema.CapNProto.Reader.Schema.CodeGeneratorRequest as CGReq

-- Testing framework imports
import Test.QuickCheck
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- Schema generation imports
import Tests.Util
import Tests.SchemaGeneration

-- Schema validation imports
import Control.Monad.Catch as C
import Control.Monad.Quota as Q

-- Functions to generate valid CGRs

generateCGR :: Schema -> IO BS.ByteString
generateCGR schema = do
  pOut <- capnpCompile (MsgMetaData (show schema) "-o-")
  return pOut

-- Functions to validate CGRs

decodeCGR :: BS.ByteString -> IO (Quota, Int)
decodeCGR bytes = do
  msg <- M.decode bytes
  (numNodes, endQuota) <- runQuotaT (rootPtr msg >>= reader) 1024
  return (endQuota, numNodes)
  where
    reader :: Maybe (Ptr BS.ByteString) -> QuotaT IO Int
    reader (Just (PtrStruct root)) = do
        let req = Schema.CodeGeneratorRequest root
        Just nodes <- CGReq.nodes req
        Just requestedFiles <- CGReq.requestedFiles req
        numNodes <- length nodes
        return numNodes
    reader _ = error "Expected `Just (PtrStruct root)`"

-- QuickCheck properties

prop_schemaValid :: Schema -> Property
prop_schemaValid schema = ioProperty $ do
  compiled <- generateCGR schema
  decoded <- try $ decodeCGR compiled
  return $ case decoded of
    Left QuotaError -> False
    Right _ -> True

schemaCGRQuickCheck = testProperty "valid schema QuickCheck" prop_schemaValid 
