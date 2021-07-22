{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import qualified Capnp                     as C
import qualified Capnp.Untyped             as U
import           Control.Monad             (unless)
import           Criterion.Main
import           System.Exit               (ExitCode(..))
import qualified System.Process.ByteString as PB

main :: IO ()
main = do
    (exit, cgrBytes, _) <- PB.readProcessWithExitCode "capnp" ["compile", "-o-", "core-schema/capnp/schema.capnp"] ""
    unless (exit == ExitSuccess) $ error "capnp compile failed"
    msg <- C.bsToMsg cgrBytes
    defaultMain
        [ bench "canonicalize" $ whnfIO $ C.evalLimitT maxBound $ do
            root <- U.rootPtr msg
            C.canonicalize root
        ]
