{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CalculatorExample (tests) where

import Data.Word
import Test.Hspec

import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Exception        (throwIO, try)
import Data.Default             (def)
import Data.Foldable            (for_)
import Data.String              (fromString)
import Network.Simple.TCP       (connect, serve)
import System.Environment       (getEnv)
import System.Exit              (ExitCode(ExitSuccess))
import System.IO.Error          (isDoesNotExistError)
import System.Process           (callProcess, readProcessWithExitCode)

import Capnp.Rpc.Transport  (socketTransport)
import Capnp.Rpc.Untyped    (ConnConfig(..), handleConn)
import Capnp.TraversalLimit (defaultLimit)

import qualified Examples.CalculatorClient
import qualified Examples.CalculatorServer

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
    clientPath <- runIO $ getExe "CXX_CALCULATOR_CLIENT"
    serverPath <- runIO $ getExe "CXX_CALCULATOR_SERVER"
    for_ clientPath $ \clientPath ->
        it "Should pass when run against our server" $
            Examples.CalculatorServer.main
                `race_` (waitForServer >> cxxClient clientPath 4000)
    for_ serverPath $ \serverPath ->
        it "Should pass when run against our client" $
            cxxServer serverPath 4000
                `race_` (waitForServer >> Examples.CalculatorClient.main)
    for_ ((,) <$> clientPath <*> serverPath) $ \(clientPath, serverPath) ->
        it "Should pass when run aginst the C++ server, proxied through us." $
            cxxServer serverPath 4000
                `race_` (waitForServer >> runProxy 4000 6000)
                -- we wait twice, so that the proxy also has time to start:
                `race_` (waitForServer >> waitForServer >> cxxClient clientPath 6000)
  where
    -- | Give the server a bit of time to start up.
    waitForServer :: IO ()
    waitForServer = threadDelay 1000000

    cxxServer :: FilePath -> Word16 -> IO ()
    cxxServer path port =
        callProcess path ["localhost:" ++ show port ]
    cxxClient :: FilePath -> Word16 -> IO ()
    cxxClient path port = do
        (eStatus, out, err) <- readProcessWithExitCode path ["localhost:" ++ show port] ""
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

-- | @'runProxy' serverPort clientPort@ connects to the server listening at
-- localhost:serverPort, requests its bootstrap interface, and then listens
-- on clientPort, offering a proxy of the server's bootstrap interface as our
-- own.
runProxy :: Word16 -> Word16 -> IO ()
runProxy serverPort clientPort =
    connect "localhost" (fromString $ show serverPort) $ \(serverSock, _addr) ->
        handleConn (socketTransport serverSock defaultLimit) def
            { debugMode = True
            , withBootstrap = Just $ \_sup client ->
                serve "localhost" (fromString $ show clientPort) $ \(clientSock, _addr) ->
                    handleConn (socketTransport clientSock defaultLimit) def
                        { getBootstrap = \_sup -> pure $ Just client
                        , debugMode = True
                        }
            }
