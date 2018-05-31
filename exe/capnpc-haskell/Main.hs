{-| This is the capnp compiler plugin.
-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Data.Capnp.Core.Schema

import Codec.Capnp               (Decerialize(..))
import Data.Capnp.TraversalLimit (evalLimitT)
import Data.Capnp.Untyped        (rootPtr)
import Data.Capnp.Untyped.Pure   (readStruct)

import System.Directory (createDirectoryIfMissing)
import System.FilePath  (takeDirectory)

import qualified Data.ByteString        as BS
import qualified Data.Capnp.Message     as Message
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO      as TIO

import qualified Backends.Pure
import qualified Backends.Raw
import qualified FrontEnd

main :: IO ()
main = do
    msg <- Message.decode =<< BS.getContents
    -- Traversal limit is 64 MiB. Somewhat aribtrary.
    cgr <- evalLimitT (64 * 1024 * 1024) (rootPtr msg >>= readStruct >>= decerialize)
    mapM_ saveResult (handleCGR cgr)
  where
    saveResult (filename, contents) = do
        createDirectoryIfMissing True (takeDirectory filename)
        TIO.writeFile filename $ TB.toLazyText contents

handleCGR :: CodeGeneratorRequest -> [(FilePath, TB.Builder)]
handleCGR = concatMap genFiles . FrontEnd.cgrToIR where
    genFiles mod =
        [ ( Backends.Pure.modFileName mod
          , Backends.Pure.fmtModule mod
          )
        , ( Backends.Raw.modFileName mod
          , Backends.Raw.fmtModule mod
          )
        ]
