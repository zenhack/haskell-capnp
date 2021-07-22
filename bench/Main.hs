{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where


import qualified Capnp                     as C
import qualified Capnp.Untyped             as U
import           Control.Monad             (unless)
import           Criterion.Main
import qualified Data.ByteString           as BS
import           System.Exit               (ExitCode(..))
import qualified System.Process.ByteString as PB

-- Get the raw bytes of a CodeGeneratorRequest for all of the bundled
-- capnproto core schema. Useful as a source of generic test data.
getCGRBytes :: IO BS.ByteString
getCGRBytes = do
    (exit, cgrBytes, _) <- PB.readProcessWithExitCode "capnp"
        [ "compile", "-o-"
        , "-I", "core-schema/"
        , "--src-prefix=core-schema/"
        , "core-schema/capnp/schema.capnp"
        , "core-schema/capnp/stream.capnp"
        , "core-schema/capnp/rpc-twoparty.capnp"
        , "core-schema/capnp/persistent.capnp"
        , "core-schema/capnp/rpc.capnp"
        , "core-schema/capnp/compat/json.capnp"
        , "core-schema/capnp/c++.capnp"
        ]
        ""
    unless (exit == ExitSuccess) $ error "capnp compile failed"
    pure cgrBytes

main :: IO ()
main = do
    cgrBytes <- getCGRBytes
    msg <- C.bsToMsg cgrBytes
    defaultMain
        [ bench "canonicalize" $ whnfIO $ C.evalLimitT maxBound $ do
            root <- U.rootPtr msg
            C.canonicalize root
        ]
