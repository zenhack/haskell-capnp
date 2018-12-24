-- This module is the main entry point for the capnpc-haskell code
-- generator plugin.
module Main (main) where

import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.Foldable    (for_)
import System.Directory (createDirectoryIfMissing)
import System.FilePath  (takeDirectory)
import System.IO        (IOMode(WriteMode), withFile)

import Capnp                       (defaultLimit, getValue)
import Capnp.Gen.Capnp.Schema.Pure (CodeGeneratorRequest)

import qualified IR.Haskell          as Haskell
import qualified Trans.CgrToStage1
import qualified Trans.FlatToRaw
import qualified Trans.HaskellToText
import qualified Trans.RawToHaskell
import qualified Trans.Stage1ToFlat

main :: IO ()
main = do
    cgr <- getValue defaultLimit
    for_ (handleCGR cgr) $ \(path, doc) -> do
        createDirectoryIfMissing True (takeDirectory path)
        withFile path WriteMode $ \h ->
            PP.hPutDoc h doc

-- | Convert a 'CodeGeneratorRequest' to a list of files to create.
handleCGR :: CodeGeneratorRequest -> [(FilePath, PP.Doc)]
handleCGR cgr =
    let modules =
            map
                ( Trans.RawToHaskell.fileToModule
                . Trans.FlatToRaw.fileToFile
                ) $
            Trans.Stage1ToFlat.filesToFiles $
            Trans.CgrToStage1.cgrToFiles cgr
    in
    map
        (\mod ->
            ( Haskell.modFilePath mod
            , Trans.HaskellToText.moduleToText mod
            )
        )
        modules

