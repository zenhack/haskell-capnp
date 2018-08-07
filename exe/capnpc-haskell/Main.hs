{-| This is the capnp compiler plugin.
-}
module Main (main) where

import Capnp.Capnp.Schema.Pure

import Codec.Capnp               (fromStruct)
import Data.Capnp.TraversalLimit (evalLimitT)
import Data.Capnp.Untyped        (rootPtr)

import System.Directory (createDirectoryIfMissing)
import System.FilePath  (takeDirectory)
import System.IO        (IOMode(WriteMode), withFile)

import qualified Data.ByteString              as BS
import qualified Data.Capnp.Message           as Message
import qualified Text.PrettyPrint.Leijen.Text as PP

import qualified Backends.Pure
import qualified Backends.Raw
import qualified FrontEnd

main :: IO ()
main = do
    msg <- Message.decode =<< BS.getContents
    -- Traversal limit is 64 MiB. Somewhat aribtrary.
    cgr <- evalLimitT (64 * 1024 * 1024) (rootPtr msg >>= fromStruct)
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
