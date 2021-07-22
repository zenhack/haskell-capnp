-- This module is the main entry point for the capnpc-haskell code
-- generator plugin.
module Main (main) where

import Data.Foldable    (for_)
import System.Directory (createDirectoryIfMissing)
import System.FilePath  (takeDirectory)
import System.IO        (IOMode(WriteMode), withFile)

import qualified Data.Text.Lazy    as LT
import qualified Data.Text.Lazy.IO as TIO

import Capnp                      (defaultLimit, getParsed)
import Capnp.Gen.Capnp.Schema.New (CodeGeneratorRequest)
import Capnp.Repr.Parsed          (Parsed)

import qualified Check
import qualified IR.Flat             as Flat
import qualified IR.Haskell          as Haskell
import qualified Trans.CgrToStage1
import qualified Trans.FlatToNew
import qualified Trans.FlatToPure
import qualified Trans.FlatToRaw
import qualified Trans.HaskellToText
import qualified Trans.NewToHaskell
import qualified Trans.PureToHaskell
import qualified Trans.RawToHaskell
import qualified Trans.Stage1ToFlat

main :: IO ()
main = do
    cgr <- getParsed defaultLimit
    Check.reportIssues cgr
    for_ (handleCGR cgr) $ \(path, contents) -> do
        createDirectoryIfMissing True (takeDirectory path)
        withFile path WriteMode $ \h ->
            TIO.hPutStr h contents

-- | Convert a 'CodeGeneratorRequest' to a list of files to create.
handleCGR :: Parsed CodeGeneratorRequest -> [(FilePath, LT.Text)]
handleCGR cgr =
    let flat =
            Trans.Stage1ToFlat.cgrToCgr $
            Trans.CgrToStage1.cgrToCgr cgr
        modules =
            concatMap ($ flat)
                [ handleFlatRaw
                , handleFlatPure
                , handleFlatNew
                ]
    in
    map
        (\mod ->
            ( Haskell.modFilePath mod
            , Trans.HaskellToText.moduleToText mod
            )
        )
        modules

handleFlatPure, handleFlatRaw, handleFlatNew :: Flat.CodeGenReq -> [Haskell.Module]

handleFlatPure =
    concatMap Trans.PureToHaskell.fileToModules
    . Trans.FlatToPure.cgrToFiles

handleFlatRaw =
    concatMap Trans.RawToHaskell.fileToModules
    . Trans.FlatToRaw.cgrToFiles

handleFlatNew =
    concatMap Trans.NewToHaskell.fileToModules
    . Trans.FlatToNew.cgrToFiles
