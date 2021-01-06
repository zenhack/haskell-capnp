{-# LANGUAGE DataKinds #-}
module PointerOOB (tests) where

import Test.Hspec

import Control.Category       ((>>>))
import Control.Exception.Safe (try)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable          (for_)
import Data.Function          ((&))

import qualified Capnp
import qualified Capnp.Errors  as E
import qualified Capnp.Message as M
import qualified Capnp.Pointer as P
import qualified Capnp.Untyped as U

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as LBS

errPtrs :: [(P.Ptr, E.Error)]
errPtrs =
    [ ( P.ListPtr 0 (P.EltNormal P.Sz1 1)
      , E.BoundsError { E.index = 2, E.maxIndex = 1 }
      )
    , ( P.ListPtr 0 (P.EltNormal P.Sz32 4)
      , E.BoundsError { E.index = 3, E.maxIndex = 1 }
      )
    , ( P.ListPtr (-3) (P.EltNormal P.Sz1 1)
      , E.BoundsError { E.index = -1, E.maxIndex = 1 }
      )
    , ( P.ListPtr (-4) (P.EltNormal P.Sz1 1)
      , E.BoundsError { E.index = -2, E.maxIndex = 1 }
      )
    ]

wrapPtr :: P.Ptr -> BS.ByteString
wrapPtr p =
    [ Just (P.StructPtr 0 0 1), Just p ]
        & map (P.serializePtr >>> BB.word64LE)
        & mconcat
        & BB.toLazyByteString
        & LBS.toStrict

tests :: Spec
tests = describe "Test correct handling of out of bound pointers" $ do
    describe "pointers that go off the end of the message" $ do
        for_ errPtrs $ \(p, err) -> do
            it ("Should catch the issue for " <> show p) $ do
                let testPtrBounds :: P.Ptr -> Capnp.LimitT IO ()
                    testPtrBounds p = do
                        let msg = M.singleSegment $ Capnp.fromByteString (wrapPtr p)
                        root <- U.rootPtr msg
                        v <- try $ U.getPtr 0 root
                        case v of
                            Right _ -> fail "should have signaled an error"
                            Left e  -> liftIO $ e `shouldBe` err
                Capnp.evalLimitT maxBound (testPtrBounds p)
