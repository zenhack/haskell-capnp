{-# LANGUAGE FlexibleContexts #-}
module Tests.Module.Data.CapNProto.List (listTests) where

import Tests.Util (assertionsToTest)

import Control.Monad.Catch.Pure       (runCatchT)
import Control.Monad.Writer           (execWriterT, tell)
import Data.CapNProto.List            (ListOf)
import Data.CapNProto.TraversalLimit  (evalWithLimit)
import Data.Functor.Identity          (Identity(..))
import Data.Maybe                     (fromJust)
import Data.Monoid                    (Sum(..))
import Schema.CapNProto.Reader.Schema (Node)
import Test.HUnit                     (assertEqual)

import qualified Data.ByteString                                     as BS
import qualified Data.CapNProto.List                                 as List
import qualified Data.CapNProto.Message                              as M
import qualified Schema.CapNProto.Reader.Schema.CodeGeneratorRequest as CGR

lengthCounted :: ListOf BS.ByteString a -> Int
lengthCounted = run . List.mapM_ (const $ tell 1)
  where
    run = getSum
        . fromRight
        . runIdentity
        . runCatchT
        . evalWithLimit 1024
        . execWriterT
    fromRight (Right x) = x
    fromRight _         = error "Left"

schemaNodes :: IO (ListOf BS.ByteString (Node BS.ByteString))
schemaNodes = do
    msg <- BS.readFile "tests/data/schema-codegenreq" >>= M.decode
    evalWithLimit 1024 (CGR.root_ msg >>= CGR.nodes)

listTests = assertionsToTest "List tests"
    [ do nodes <- schemaNodes
         -- First, sanity check that List.length returns the value
         -- we expect:
         assertEqual
            "List.length nodes == 37"
                (List.length nodes)
                37
         -- OK, now, make sure that we actually see that many items
         -- when we mapM_ over it:
         assertEqual
            "List.length nodes == lengthCounted nodes"
                (List.length nodes)
                (lengthCounted nodes)
    ]
