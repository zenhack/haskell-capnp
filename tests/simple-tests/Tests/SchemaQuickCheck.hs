module Tests.SchemaQuickCheck
    (schemaCGRQuickCheck)
  where

import qualified Data.ByteString as BS

import           Data.CapNProto.Blob                                 (BlobSlice)
import           Data.CapNProto.Message                              as M
import qualified Data.CapNProto.Untyped                              as Untyped
import           Schema.CapNProto.Reader.Schema                      as Schema hiding
    (Field)
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
import Control.Monad.Quota as Q

-- Functions to generate valid CGRs

generateCGR :: Schema -> IO BS.ByteString
generateCGR schema = do
  pOut <- capnpCompile (MsgMetaData (show schema) "-o-")
  return pOut

-- Functions to validate CGRs

decodeCGR :: BS.ByteString -> IO (Quota, Int)
decodeCGR bytes = do
  let reader :: Untyped.Struct BS.ByteString -> QuotaT IO Int
      reader struct = do
        let req = Schema.CodeGeneratorRequest struct
        Just nodes <- CGReq.nodes req
        Just requestedFiles <- CGReq.requestedFiles req
        return (Untyped.length nodes)
  msg <- M.decode bytes
  (numNodes, endQuota) <- runQuotaT (Untyped.rootPtr msg >>= reader) 1024
  return (endQuota, numNodes)

-- QuickCheck properties

prop_schemaValid :: Schema -> Property
prop_schemaValid schema = ioProperty $ do
  compiled <- generateCGR schema
  decoded <- try $ decodeCGR compiled
  return $ case decoded of
    Left QuotaError -> False
    Right _         -> True

schemaCGRQuickCheck = testProperty "valid schema QuickCheck"
                      (prop_schemaValid <$> genSchema)
