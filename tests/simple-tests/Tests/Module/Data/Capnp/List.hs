{-# LANGUAGE FlexibleContexts #-}
module Tests.Module.Data.Capnp.List (listTests) where

import Tests.Util (assertionsToTest)

import Control.Monad.Writer      (MonadWriter, execWriterT, runWriterT, tell)
import Data.Capnp.List           (ListOf)
import Data.Capnp.TraversalLimit (evalLimitT)
import Data.Monoid               (Sum(..))
import Test.HUnit                (assertEqual)

import qualified Data.ByteString                   as BS
import qualified Data.Capnp.ById.Xa93fc509624c72d9 as Schema
import qualified Data.Capnp.List                   as List
import qualified Data.Capnp.Message                as M
import qualified Data.Capnp.Untyped                as Untyped

lengthCounted :: (MonadWriter (Sum Int) m, Untyped.ReadCtx m b) => ListOf m b a -> m ()
lengthCounted = List.mapM_ (const $ tell 1)

readSchema :: IO (M.Message BS.ByteString)
readSchema =
    BS.readFile "tests/data/schema-codegenreq" >>= M.decode

schemaNodes :: Untyped.ReadCtx m b => M.Message b -> m (Untyped.ListOf m b (Schema.Node m b))
schemaNodes msg = do
    cgr <- Schema.CodeGeneratorRequest <$> Untyped.rootPtr msg
    Schema.get_CodeGeneratorRequest'nodes cgr

listTests = assertionsToTest "List tests"
    [ do msg <- readSchema
         (nodes, _) <- evalLimitT 1024 $ runWriterT $ schemaNodes msg
         -- First, sanity check that List.length returns the value
         -- we expect:
         assertEqual
            "List.length nodes == 37"
                (List.length nodes)
                37
         -- OK, now, make sure that we actually see that many items
         -- when we mapM_ over it:
         counted <- getSum <$> evalLimitT 1024 (execWriterT $ lengthCounted nodes)
         assertEqual
            "List.length nodes == lengthCounted nodes"
                (List.length nodes)
                counted
    ]
