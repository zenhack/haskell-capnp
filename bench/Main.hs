{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Capnp as C
import Capnp.Mutability (thaw)
import qualified Capnp.Untyped as U
import Control.DeepSeq (NFData (..))
import Control.Monad (unless)
import Criterion.Main
import qualified Data.ByteString as BS
import System.Exit (ExitCode (..))
import qualified System.Process.ByteString as PB

-- Get the raw bytes of a CodeGeneratorRequest for all of the bundled
-- capnproto core schema. Useful as a source of generic test data.
getCGRBytes :: IO BS.ByteString
getCGRBytes = do
  (exit, cgrBytes, _) <-
    PB.readProcessWithExitCode
      "capnp"
      [ "compile",
        "-o-",
        "-I",
        "core-schema/",
        "--src-prefix=core-schema/",
        "core-schema/capnp/schema.capnp",
        "core-schema/capnp/stream.capnp",
        "core-schema/capnp/rpc-twoparty.capnp",
        "core-schema/capnp/persistent.capnp",
        "core-schema/capnp/rpc.capnp",
        "core-schema/capnp/compat/json.capnp",
        "core-schema/capnp/c++.capnp"
      ]
      ""
  unless (exit == ExitSuccess) $ error "capnp compile failed"
  pure cgrBytes

instance NFData (C.Message mut) where
  rnf = (`seq` ())

main :: IO ()
main = do
  cgrBytes <- getCGRBytes
  msg <- C.bsToMsg cgrBytes
  let whnfLTIO = whnfIO . C.evalLimitT maxBound
  defaultMain
    [ bench "canonicalize/IO" $ whnfLTIO $ do
        root <- U.rootPtr msg
        C.canonicalize root,
      bench "canonicalize/PureBuilder" $ whnfLTIO $ do
        C.createPure maxBound $ do
          root <- U.rootPtr msg
          (msg, _seg) <- C.canonicalize root
          pure msg,
      env
        ( C.evalLimitT maxBound $ do
            mutMsg <- thaw msg
            newMsg <- C.newMessage Nothing
            pure (mutMsg, newMsg)
        )
        ( \ ~(mutMsg, newMsg) -> bench "copy" $ whnfLTIO $ do
            root <- U.rootPtr mutMsg
            U.copyPtr newMsg (Just (U.PtrStruct root))
        )
    ]
