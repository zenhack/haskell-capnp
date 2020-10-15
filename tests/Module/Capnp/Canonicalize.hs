module Module.Capnp.Canonicalize
    ( canonicalizeTests
    ) where

import Test.Hspec
import Test.QuickCheck    (property)
import Test.QuickCheck.IO (propertyIO)

import qualified Data.ByteString.Lazy as LBS

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
            property $ propertyIO . implsAgreeOn

implsAgreeOn :: PU.Struct -> IO ()
implsAgreeOn struct = do
    let Just ourMsg = ourImplCanonicalize struct
    refMsg <- refImplCanonicalize struct
    unless (ourMsg == refMsg) $
        error $ "Our implementation disagrees with the reference implementation on " ++ show struct

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
