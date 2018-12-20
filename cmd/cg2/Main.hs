module Main (main) where

import qualified Data.Text as T

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
handleCGR = mapM_ putStrLn
    . map
        ( T.unpack
        . Haskell.format
        . Trans.FlatToHaskell.fileToModule
        . Trans.Stage1ToFlat.fileToFile
        )
    . Trans.CgrToStage1.cgrToFiles
