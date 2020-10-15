{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}
module Module.Capnp.Canonicalize
    ( canonicalizeTests
    ) where

import Test.Hspec
import Test.QuickCheck    (property)
import Test.QuickCheck.IO (propertyIO)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector          as V

import Control.Monad (unless)

import Capnp.Canonicalize

import Capnp (cerialize, createPure, msgToLBS)

import qualified Capnp.Message      as M
import qualified Capnp.Untyped      as U
import qualified Capnp.Untyped.Pure as PU

import Instances ()
import Util      (capnpCanonicalize)

canonicalizeTests :: Spec
canonicalizeTests =
    describe "canonicalization tests" $ do
        it "agrees with reference implementation" $
            property $ \case
                PU.Struct (PU.Slice (V.toList -> [])) (PU.Slice (V.toList -> [])) ->
                    -- skip this; it fails due to a bug in the reference implementation:
                    --
                    -- https://github.com/capnproto/capnproto/issues/1084
                    --
                    -- TODO: when that issue is fixed, stop skipping this case.
                    propertyIO $ pure ()
                struct ->
                    propertyIO $ implsAgreeOn struct

implsAgreeOn :: PU.Struct -> IO ()
implsAgreeOn struct = do
    let Just ourMsg = ourImplCanonicalize struct
    refMsg <- refImplCanonicalize struct
    unless (ourMsg == refMsg) $
        error $ concat
            [ "Our implementation disagrees with the reference implementation on " ++ show struct
            , ".\n\nWe produce:\n\n"
            , show $ msgToLBS ourMsg
            , "\n\n"
            , "But the reference implementation generates:\n\n"
            , show $ msgToLBS refMsg
            ]

ourImplCanonicalize :: PU.Struct -> Maybe M.ConstMsg
ourImplCanonicalize struct = createPure maxBound $ do
    msg <- M.newMessage Nothing
    rawStruct <- cerialize msg struct
    (msg, _) <- canonicalize rawStruct
    pure msg

refImplCanonicalize :: PU.Struct -> IO M.ConstMsg
refImplCanonicalize struct = do
    msg <- createPure maxBound $ do
        msg <- M.newMessage Nothing
        rawStruct <- cerialize msg struct
        U.setRoot rawStruct
        pure msg
    lbs <- capnpCanonicalize (msgToLBS msg)
    segment <- M.fromByteString $ LBS.toStrict lbs
    pure $ M.singleSegment segment
