module Main (main) where

import qualified Text.PrettyPrint.Leijen.Text as PP

import System.IO (stdout)

import Capnp                       (defaultLimit, getValue)
import Capnp.Gen.Capnp.Schema.Pure (CodeGeneratorRequest)

import qualified IR.Haskell          as Haskell
import qualified Trans.CgrToStage1
import qualified Trans.FlatToHaskell
import qualified Trans.Stage1ToFlat

main :: IO ()
main = do
    cgr <- getValue defaultLimit
    handleCGR cgr

handleCGR :: CodeGeneratorRequest -> IO ()
handleCGR =
    PP.hPutDoc stdout
    . PP.vcat
    . map
        ( Haskell.format
        . Trans.FlatToHaskell.fileToModule
        . Trans.Stage1ToFlat.fileToFile
        )
    . Trans.CgrToStage1.cgrToFiles
