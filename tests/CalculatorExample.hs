{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CalculatorExample (tests) where

import Test.Hspec

import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Exception        (SomeException, try)
import Data.Foldable            (for_)
import System.Environment       (getEnv)
import System.Exit              (ExitCode(ExitSuccess))
import System.Process           (readProcessWithExitCode)

import qualified Examples.CalculatorServer

tests :: Spec
tests = describe "Check our example against the C++ implementation" $ do
    cxxPath <- runIO $
        try (getEnv "CXX_CALCULATOR_CLIENT") >>= \case
            -- XXX TODO: only catch the exception that getEnv throws:
            Left (_ :: SomeException) -> do
                putStrLn "CXX_CALCULATOR_CLIENT not sent; skipping."
                pure Nothing
            Right path ->
                pure (Just path)
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
