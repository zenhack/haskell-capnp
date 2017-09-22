module Tests.SchemaQuickCheck
    (schemaCGRQuickCheck)
  where

import Test.QuickCheck

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.ByteString as BS
import Control.Monad.Quota as Q
import Control.Monad.Catch as C
import qualified Data.Vector as V
import Data.CapNProto.Message as M
import Data.CapNProto.Blob as B

decodeCGRLen :: (MonadThrow m) => BS.ByteString -> m Int
decodeCGRLen bytes = do
  (Message segs) <- M.decode bytes
  return $ BS.length $ BS.concat $ V.toList $ V.map B.blob segs

prop_schemaValid :: String -> Property
prop_schemaValid filename = ioProperty $ do
  bytes <- BS.readFile filename
  decoded <- try $ decodeCGRLen bytes
  return $ r === decoded
  where r | filename == "tests/data/schema-codegenreq" = Right 73152
          | otherwise = Left QuotaError

schemaCGRQuickCheck = testProperty "valid schema QuickCheck" $ prop_schemaValid "tests/data/schema-codegenreq"
