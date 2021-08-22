{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
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
import Data.Function             ((&))

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

import Capnp.New
    (Raw, bsToRaw, hasField, index, length, parseField, readField, textBytes)
import Capnp.TraversalLimit (LimitT, evalLimitT, execLimitT)

import qualified Capnp.Gen.Capnp.Schema.New as Schema
import qualified Capnp.Message              as M

nodeNames :: V.Vector BS.ByteString
nodeNames = V.fromList
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
            root <- evalLimitT maxBound (bsToRaw bytes)
            endQuota <- execLimitT 4096 (reader root)
            endQuota `shouldBe` 3409
  where
    reader :: Raw Schema.CodeGeneratorRequest 'M.Const -> LimitT IO ()
    reader req = do
        nodes <- req & readField #nodes
        requestedFiles <- req & readField #requestedFiles
        lift $ length nodes `shouldBe` 37
        lift $ length requestedFiles `shouldBe` 1
        mapM_ (walkNode nodes) [0,1..36]
    walkNode nodes i = do
        node <- index i nodes
        -- None of the nodes in the schema have parameters:
        False <- node & hasField #parameters
        -- And none of them are generic:
        False <- node & parseField #isGeneric

        nameList <- node & readField #displayName
        name <- textBytes nameList
        prefixLen <- parseField #displayNamePrefixLength node
        let baseName = BS.drop (fromIntegral prefixLen) name

        when (i < V.length nodeNames && baseName /= (nodeNames V.! i)) $
            error "Incorrect name."

        has <- node & hasField #annotations

        -- there are two annotations in all of the nodes, at these indicies:
        case (has, i `elem` [4, 9]) of
            (False, False) -> return ()
            (True, True) -> do
                1 <- length <$> readField #annotations node
                return ()
            (False, True) ->
                error $ "Node at index " ++ show i ++ " should have had" ++
                        "an annotation."
            (True, False) ->
                error $ "Node at index " ++ show i ++ " should not " ++
                        "have had an annotation."
