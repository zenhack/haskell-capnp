-- | This module defines a test that tries to walk over the
-- CodeGeneratorRequest in `tests/data/schema-codegenreq`,
-- failing if any of the data is not as expected.
module Tests.WalkSchemaCodeGenRequest
    (walkSchemaCodeGenRequestTest)
  where

import Prelude hiding (length)

import Control.Monad.Quota
import Data.CapNProto.Untyped
import Tests.Util

import Control.Monad  (mapM_)
import Test.Framework (Test)
import Test.HUnit     (Assertion, assertEqual)

import qualified Data.ByteString                                     as BS
import qualified Data.CapNProto.Message                              as M
import qualified Schema.CapNProto.Reader.Schema                      as Schema
import qualified Schema.CapNProto.Reader.Schema.CodeGeneratorRequest as CGReq
import qualified Schema.CapNProto.Reader.Schema.Node                 as Node

-- TODO: This contains a bit of copypasta from some of the untyped tests; should
-- factor that out.
theAssert :: Assertion
theAssert = do
    bytes <- BS.readFile "tests/data/schema-codegenreq"
    msg <- M.decode bytes
    ((), endQuota) <- runQuotaT (rootPtr msg >>= reader) 1024
    assertEqual "Correct remaining quota" 791 endQuota
  where
    reader :: Maybe (Ptr BS.ByteString) -> QuotaT IO ()
    reader (Just (PtrStruct root)) = do
        let req = Schema.CodeGeneratorRequest root
        Just nodes <- CGReq.nodes req
        Just requestedFiles <- CGReq.requestedFiles req
        37 <- length nodes
        1 <- length requestedFiles
        mapM_ (walkNode nodes) [0,1..36]
    reader _ = error "Expected `Just (PtrStruct root)`"
    walkNode nodes i = do
        node <- index i nodes
        -- None of the nodes in the schema have parameters:
        Nothing <- Node.parameters node
        -- And none of them are generic:
        False <- Node.isGeneric node

        annotations <- Node.annotations node

        -- there are two annotations in all of the nodes, at these indicies:
        case (annotations, i `elem` [4, 9]) of
            (Nothing, False) -> return ()
            (Just annotations', True) -> do
                1 <- length annotations'
                return ()
            (Nothing, True) ->
                error $ "Node at index " ++ show i ++ " should have had" ++
                        "an annotation."
            (Just _, False) ->
                error $ "Node at index " ++ show i ++ " should not " ++
                        "have had an annotation."

walkSchemaCodeGenRequestTest :: Test
walkSchemaCodeGenRequestTest =
    assertionsToTest "walk schema CodeGenerationRequest" [theAssert]
