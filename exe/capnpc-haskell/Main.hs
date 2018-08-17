{-| This is the capnp compiler plugin.
-}
module Main (main) where

import Capnp.Capnp.Schema.Pure (CodeGeneratorRequest)

import Data.Capnp (decodeMessage, defaultLimit, evalLimitT, getRoot)

import System.Directory (createDirectoryIfMissing)
import System.FilePath  (takeDirectory)
import System.IO        (IOMode(WriteMode), withFile)

import qualified Data.ByteString              as BS
import qualified Text.PrettyPrint.Leijen.Text as PP

import qualified Backends.Pure
import qualified Backends.Raw
import qualified FrontEnd

main :: IO ()
main = do
    msg <- decodeMessage =<< BS.getContents
    cgr <- evalLimitT defaultLimit (getRoot msg)
    mapM_ saveResult (handleCGR cgr)
  where
    saveResult (filename, contents) = do
        createDirectoryIfMissing True (takeDirectory filename)
        withFile filename WriteMode $ \h ->
            PP.hPutDoc h contents

handleCGR :: CodeGeneratorRequest -> [(FilePath, PP.Doc)]
handleCGR = concatMap genFiles . FrontEnd.cgrToIR where
    genFiles mod =
        Backends.Pure.fmtModule mod ++ Backends.Raw.fmtModule mod
