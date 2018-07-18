{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module defines a test that tries to walk over the
-- CodeGeneratorRequest in `tests/data/schema-codegenreq`,
-- failing if any of the data is not as expected.
module Tests.WalkSchemaCodeGenRequest
    (walkSchemaCodeGenRequestTest)
  where

import Prelude hiding (length)

import Data.Capnp.Untyped hiding (index, length)
import Tests.Util

import Control.Monad             (mapM_, when)
import Data.Capnp.Basics         (Text(..), index, length)
import Data.Capnp.TraversalLimit (LimitT, execLimitT)
import Test.Framework            (Test)
import Test.HUnit                (Assertion, assertEqual)

import qualified Capnp.Capnp.Schema as Schema
import qualified Data.ByteString    as BS
import qualified Data.Capnp.Message as M
import qualified Prelude


-- | TODO: make this an array; we're doing random access to it below.
-- I(@zenhack) am waiting on this, since at the time of writing @taktoa
-- is working on some array utilities that will get merged soonish, so
-- it probably makes sense to just wait for that.
nodeNames :: [BS.ByteString]
nodeNames =
    [ "Import"
    , "annotation"
    , "Value"
    , "Type"
    ]

-- TODO: This contains a bit of copypasta from some of the untyped tests; should
-- factor that out.
theAssert :: Assertion
theAssert = do
    bytes <- BS.readFile "tests/data/schema-codegenreq"
    msg <- M.decode bytes
    endQuota <- execLimitT 4096 (rootPtr msg >>= reader)
    assertEqual "Correct remaining quota" 2036 endQuota
  where
    reader :: Struct M.ConstMsg -> LimitT IO ()
    reader root = do
        let req = Schema.CodeGeneratorRequest root
        nodes <- Schema.get_CodeGeneratorRequest'nodes req
        requestedFiles <- Schema.get_CodeGeneratorRequest'requestedFiles req
        let 37 = length nodes
        let 1 = length requestedFiles
        mapM_ (walkNode nodes) [0,1..36]
    walkNode nodes i = do
        node <- index i nodes
        -- None of the nodes in the schema have parameters:
        False <- Schema.has_Node''parameters node
        -- And none of them are generic:
        False <- Schema.get_Node''isGeneric node

        Text nameList <- Schema.get_Node''displayName node
        name <- rawBytes nameList
        prefixLen <- Schema.get_Node''displayNamePrefixLength node
        let baseName = BS.drop (fromIntegral prefixLen) name

        when (i < Prelude.length nodeNames && baseName /= (nodeNames !! i)) $
            error "Incorrect name."

        has <- Schema.has_Node''annotations node

        -- there are two annotations in all of the nodes, at these indicies:
        case (has, i `elem` [4, 9]) of
            (False, False) -> return ()
            (True, True) -> do
                1 <- length <$> Schema.get_Node''annotations node
                return ()
            (False, True) ->
                error $ "Node at index " ++ show i ++ " should have had" ++
                        "an annotation."
            (True, False) ->
                error $ "Node at index " ++ show i ++ " should not " ++
                        "have had an annotation."

walkSchemaCodeGenRequestTest :: Test
walkSchemaCodeGenRequestTest =
    assertionsToTest "walk schema CodeGenerationRequest" [theAssert]
