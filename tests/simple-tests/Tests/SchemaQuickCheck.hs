module Tests.SchemaQuickCheck
    (schemaCGRQuickCheck)
  where

import qualified Data.ByteString as BS

import Prelude hiding (length)
import Data.CapNProto.Untyped (length, Ptr(PtrStruct), rootPtr)
import Data.CapNProto.Blob (BlobSlice)
import Data.CapNProto.Message as M
import Data.CapNProto.Schema   (Field(..))
import Schema.CapNProto.Reader.Schema as Schema
import qualified Schema.CapNProto.Reader.Schema.CodeGeneratorRequest as CGReq

-- Testing framework imports
import Test.QuickCheck
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- Schema generation imports
import Tests.Util

-- Schema validation imports
import Control.Monad.Catch as C
import Control.Monad.Quota as Q

-- Functions to generate valid CGRs

generateCGR fields = do
  pOut <- capnpCompile (MsgMetaData (createSchema fields) "-o-")
  return pOut
  where
    createSchema :: [(String, String, String)] -> String
    createSchema fields =
      "@0x832bcc6686a26d56;\n\n" ++ "struct Thing {\n" ++ (build fields) ++ "}"
      where
        build ((fname, fver, ftype):ff) =
          fname ++ " @" ++ fver ++ " :" ++ ftype ++ ";\n" ++ (build ff)
        build _ = ""

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

prop_schemaValid :: String -> Property
prop_schemaValid filename = ioProperty $ do
  bytes <- BS.readFile filename
  decoded <- try $ decodeCGR bytes
  return $ r === decoded
  where r | filename == "tests/data/schema-codegenreq" = Right (Quota 1016, 37)
          | otherwise = Left QuotaError

schemaCGRQuickCheck = testProperty "valid schema QuickCheck" $ prop_schemaValid "tests/data/schema-codegenreq"
