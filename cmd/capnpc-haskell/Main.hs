-- This module is the main entry point for the capnpc-haskell code
-- generator plugin.
module Main (main) where

import Capnp (Parsed, defaultLimit, getParsed)
import Capnp.Gen.Capnp.Schema (CodeGeneratorRequest)
import qualified Check
import Control.Category ((>>>))
import Data.Foldable (for_)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as TIO
import qualified IR.Haskell as Haskell
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (IOMode (WriteMode), withFile)
import qualified Trans.AbstractOpToHaskell
import qualified Trans.CgrToStage1
import qualified Trans.FlatToAbstractOp
import qualified Trans.HaskellToText
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
handleCGR =
  Trans.CgrToStage1.cgrToCgr
    >>> Trans.Stage1ToFlat.cgrToCgr
    >>> Trans.FlatToAbstractOp.cgrToFiles
    >>> concatMap Trans.AbstractOpToHaskell.fileToModules
    >>> map
      ( \mod ->
          ( Haskell.modFilePath mod,
            Trans.HaskellToText.moduleToText mod
          )
      )
