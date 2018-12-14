{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CalculatorExample (tests) where

import Test.Hspec

import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Exception        (throwIO, try)
import Data.Foldable            (for_)
import System.Environment       (getEnv)
import System.Exit              (ExitCode(ExitSuccess))
import System.IO.Error          (isDoesNotExistError)
import System.Process           (readProcessWithExitCode)

import qualified Examples.CalculatorServer

getCxxClient :: IO (Maybe FilePath)
getCxxClient = getExe "CXX_CALCULATOR_CLIENT"

getExe :: String -> IO (Maybe FilePath)
getExe varName =
    try (getEnv varName) >>= \case
        Left e
            | isDoesNotExistError e -> do
                putStrLn $ varName ++ " not set; skipping."
                pure Nothing
            | otherwise ->
                throwIO e
        Right path ->
            pure (Just path)

tests :: Spec
tests = describe "Check our example against the C++ implementation" $ do
    cxxPath <- runIO $ getCxxClient
    for_ cxxPath $ \path ->
        it "Should pass when run against our server" $
            race_
                Examples.CalculatorServer.main
                (do
                    -- Give the server plenty of time to start up:
                    threadDelay 2000000

                    (eStatus, out, err) <- readProcessWithExitCode path ["localhost:4000"] ""
                    (eStatus, out, err)
                        `shouldBe`
                        ( ExitSuccess
                        , unlines
                            [ "Evaluating a literal... PASS"
                            , "Using add and subtract... PASS"
                            , "Pipelining eval() calls... PASS"
                            , "Defining functions... PASS"
                            , "Using a callback... PASS"
                            ]
                        , ""
                        )
                )
