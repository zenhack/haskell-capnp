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
import qualified Trans.FlatToHaskell
import qualified Trans.Stage1ToFlat

main :: IO ()
main = do
    cgr <- getValue defaultLimit
    for_ (handleCGR cgr) $ \(path, doc) -> do
        createDirectoryIfMissing True (takeDirectory path)
        withFile path WriteMode $ \h ->
            PP.hPutDoc h doc

handleCGR :: CodeGeneratorRequest -> [(FilePath, PP.Doc)]
handleCGR cgr =
    let modules =
            map Trans.FlatToHaskell.fileToModule $
            map Trans.Stage1ToFlat.fileToFile $
            Trans.CgrToStage1.cgrToFiles cgr
    in
    map
        (\mod -> (Haskell.modFilePath mod, Haskell.format mod))
        modules

