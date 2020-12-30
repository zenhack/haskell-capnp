{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module defines a test that tries to walk over the
-- CodeGeneratorRequest in `tests/data/schema-codegenreq`,
-- failing if any of the data is not as expected.
module WalkSchemaCodeGenRequest
    (walkSchemaCodeGenRequestTest)
  where

import Prelude hiding (length)

import Test.Hspec

import Control.Monad             (when)
import Control.Monad.Trans.Class (lift)

import qualified Data.ByteString as BS
import qualified Prelude

import Capnp.Untyped hiding (index, length)

import Capnp.Basics         (index, length, textBytes)
import Capnp.Classes        (fromStruct)
import Capnp.TraversalLimit (LimitT, execLimitT)

import qualified Capnp.Gen.Capnp.Schema as Schema
import qualified Capnp.Message          as M


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
walkSchemaCodeGenRequestTest :: Spec
walkSchemaCodeGenRequestTest =
    describe "Various sanity checks on a known schema CodeGeneratorRequest" $
        it "Should match misc. expectations" $ do
            -- TODO: the above description betrays that this test isn't
            -- especially well defined at a granularity that I(zenhack)
            -- know how to tell hspec about, because there are data
            -- dependencies between conceptual tests (we walk over the
            -- data checking various things as we go).
            --
            -- It would be nice if we could mark off individual checks for
            -- hspec's reporting somehow.
            bytes <- BS.readFile "tests/data/schema-codegenreq"
            msg <- M.decode bytes
            endQuota <- execLimitT 4096 (rootPtr msg >>= reader)
            endQuota `shouldBe` 3407
  where
    reader :: Struct 'M.Const -> LimitT IO ()
    reader root = do
        req :: Schema.CodeGeneratorRequest 'M.Const <- fromStruct root
        nodes <- Schema.get_CodeGeneratorRequest'nodes req
        requestedFiles <- Schema.get_CodeGeneratorRequest'requestedFiles req
        lift $ length nodes `shouldBe` 37
        lift $ length requestedFiles `shouldBe` 1
        mapM_ (walkNode nodes) [0,1..36]
    walkNode nodes i = do
        node <- index i nodes
        -- None of the nodes in the schema have parameters:
        False <- Schema.has_Node'parameters node
        -- And none of them are generic:
        False <- Schema.get_Node'isGeneric node

        nameList <- Schema.get_Node'displayName node
        name <- textBytes nameList
        prefixLen <- Schema.get_Node'displayNamePrefixLength node
        let baseName = BS.drop (fromIntegral prefixLen) name

        when (i < Prelude.length nodeNames && baseName /= (nodeNames !! i)) $
            error "Incorrect name."

        has <- Schema.has_Node'annotations node

        -- there are two annotations in all of the nodes, at these indicies:
        case (has, i `elem` [4, 9]) of
            (False, False) -> return ()
            (True, True) -> do
                1 <- length <$> Schema.get_Node'annotations node
                return ()
            (False, True) ->
                error $ "Node at index " ++ show i ++ " should have had" ++
                        "an annotation."
            (True, False) ->
                error $ "Node at index " ++ show i ++ " should not " ++
                        "have had an annotation."
