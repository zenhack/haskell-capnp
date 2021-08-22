{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module SchemaQuickCheck
    (schemaCGRQuickCheck)
    where

import qualified Data.ByteString as BS

import Capnp.Convert        (bsToParsed)
import Capnp.Errors         (Error)
import Capnp.TraversalLimit (defaultLimit, evalLimitT)

import qualified Capnp.Gen.Capnp.Schema.New as Schema

-- Testing framework imports
import Test.Hspec
import Test.QuickCheck

-- Schema generation imports
import SchemaGeneration
import Util

-- Schema validation imports
import Control.Monad.Catch as C

-- Functions to generate valid CGRs

generateCGR :: Schema -> IO BS.ByteString
generateCGR schema = capnpCompile (show schema) "-"

-- Functions to validate CGRs

decodeCGR :: BS.ByteString -> IO ()
decodeCGR bytes = do
    _ <- evalLimitT defaultLimit (bsToParsed @Schema.CodeGeneratorRequest bytes)
    pure ()

-- QuickCheck properties

prop_schemaValid :: Schema -> Property
prop_schemaValid schema = ioProperty $ do
    compiled <- generateCGR schema
    decoded <- try $ decodeCGR compiled
    return $ case (decoded :: Either Error ()) of
        Left _  -> False
        Right _ -> True

schemaCGRQuickCheck :: Spec
schemaCGRQuickCheck =
    describe "generateCGR an decodeCGR agree" $
        it "successfully decodes generated schema" $
            property $ prop_schemaValid <$> genSchema
