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

generateCGR :: [(String, String, String)] -> IO BS.ByteString
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

-- Schema generators

genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genFieldName :: Gen String
genFieldName = listOf1 genSafeChar

newtype FieldName = FieldName { unwrapFieldName :: String }
    deriving Show

instance Arbitrary FieldName where
    arbitrary = FieldName <$> genFieldName

-- QuickCheck properties

prop_schemaValid :: [FieldName] -> Property
prop_schemaValid fieldNames = ioProperty $ do
  compiled <- generateCGR [(unwrapFieldName fn, show i, "Text") | (i, fn) <- zip [0..] fieldNames]
  decoded <- try $ decodeCGR compiled
  return $ case decoded of
    Left QuotaError -> False
    Right _ -> True

schemaCGRQuickCheck = testProperty "valid schema QuickCheck" prop_schemaValid
