{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module: Data.CapNProto.Untyped
Description: Utilities for manipulating capnproto messages with no schema.

The types and functions in this module know about things like structs and
lists, but are not schema aware.
-}
module Data.CapNProto.Untyped where

import Prelude hiding (lookup, length)
import Data.Word
import Data.CapNProto.Address (WordAddr(..))
import Data.CapNProto.List
import qualified Data.CapNProto.Pointer as P
import Data.CapNProto.Pointer (EltSpec(..), ElementSize(..))
import Control.Monad.Catch (MonadThrow, throwM, Exception)
import Control.Monad.Quota


-- This is a WIP, still thinking about the design of this.
-- The idea is to have a series of types that abstract out
-- the "cursor".

data Struct m = Struct (m (List m Word64, List m (Ptr m)))


data Ptr m
    = StructPtr (m (Struct m))
    | CapPtr (m Word32)
    | FarPtr (m (Ptr m))
    | ListPtr (m (ListPtr m))


data ListPtr m
    = List0  (List m ())
    | List1  (List m Bool)
    | List8  (List m Word8)
    | List16 (List m Word16)
    | List32 (List m Word32)
    | List64 (List m Word64)
    | ListP
        Bool -- true iff this is really a composite
        (List m (Ptr m))


type Message m = List m (List m Word64)


data IllegalMessageError
    = ErrorRootIsNotStruct
    | ErrorIllegalLandingPad P.Ptr
    | ErrorIllegalTagWord P.Ptr
    deriving(Show, Eq)

instance Exception IllegalMessageError


getWord :: (Monad m) => Message m -> WordAddr -> m Word64
getWord list (WordAt seg off) = do
    segment <- lookup list seg
    lookup segment off


-- | @getNearPtr msg addr@ treats the word at @addr@ as a pointer,
-- and follows it as far as it needs to to find a pointer that is *not*
-- a FarPtr.
getNearPtr :: (Monad m, MonadThrow m, MonadQuota m)
    => Message m -> WordAddr -> m (Ptr m)
getNearPtr msg addr = getPtr msg addr >>= followToNear msg
  where
    followToNear msg (FarPtr ptr) = ptr >>= followToNear msg
    followToNear msg ptr = return ptr


getRootStruct :: (Monad m, MonadThrow m, MonadQuota m)
    => Message m -> m (Struct m)
getRootStruct msg = do
    ptr <- getNearPtr msg (WordAt 0 0)
    case ptr of
        StructPtr struct -> struct
        _ -> throwM ErrorRootIsNotStruct


getStruct :: (Monad m, MonadThrow m, MonadQuota m)
    => Message m -> WordAddr -> Word16 -> Word16 -> m (Struct m)
getStruct msg (WordAt seg off) dataSz ptrSz = return $ Struct $ do
    segment <- lookup msg seg
    return $
        ( slice segment
                off
                (off + fromIntegral dataSz)
        , List { length = return (fromIntegral ptrSz)
               , lookup = \i -> do
                    checkBounds i (fromIntegral ptrSz)
                    let ptrOff = off + 1 + fromIntegral dataSz + i
                    getPtr msg (WordAt seg ptrOff)
               }
        )


getPtr :: (Monad m, MonadThrow m, MonadQuota m)
    => Message m -> WordAddr -> m (Ptr m)
getPtr msg addr@(WordAt seg off) = do
    word <- getWord msg addr
    case P.parsePtr word of
        P.StructPtr off' dataSz ptrSz -> return $ StructPtr $
            getStruct msg (WordAt seg (off + 1 + fromIntegral off')) dataSz ptrSz
        P.CapPtr cap -> return (CapPtr (return cap))
        P.ListPtr off' eltSpec ->
            return $ ListPtr $ getListPtr
                                msg
                                (WordAt (fromIntegral seg)
                                        (off + 1 + fromIntegral off'))
                                eltSpec
        P.FarPtr False seg' off' ->
            return $ FarPtr $ getPtr
                                msg
                                (WordAt (fromIntegral seg')
                                        (fromIntegral off'))
        P.FarPtr True seg' off' ->
            return $ FarPtr $ getLandingPad
                                msg
                                (WordAt (fromIntegral seg')
                                        (fromIntegral off'))


getLandingPad :: (Monad m, MonadThrow m, MonadQuota m)
    => Message m -> WordAddr -> m (Ptr m)
getLandingPad msg addr@(WordAt seg off) = do
    landing <- getWord msg addr
    tagWord <- getWord msg (WordAt seg (off + 1))
    case P.parsePtr landing of
        P.FarPtr False off' seg' -> case P.parsePtr tagWord of
            P.StructPtr _ dataSz ptrSz ->
                return $ StructPtr $ getStruct msg (WordAt (fromIntegral seg')
                                                           (fromIntegral off'))
                                                   dataSz ptrSz
            P.ListPtr _ eltSpec ->
                return $ ListPtr $ getListPtr
                                        msg
                                        (WordAt (fromIntegral seg')
                                                (fromIntegral off'))
                                        eltSpec
            tagPtr -> throwM $ ErrorIllegalTagWord tagPtr
            _ -> error "Not implemented"
        ptr -> throwM $ ErrorIllegalLandingPad ptr


getCompositeList :: (Monad m, MonadThrow m, MonadQuota m)
    => Message m -> WordAddr -> Int -> m (List m (Ptr m))
getCompositeList msg addr@(WordAt seg off) totalLen = do
    ptr <- P.parsePtr <$> getWord msg addr
    case ptr of
        P.StructPtr _ dataSz ptrSz -> do
            let Just eltSz = P.targetSize ptr
            let len = totalLen `div` eltSz
            return $ List { length = return len
                          , lookup = \i -> do
                                checkBounds i len
                                return $ StructPtr $
                                    getStruct
                                        msg
                                        (WordAt
                                            seg
                                            (off + 1 + eltSz * i))
                                        dataSz
                                        ptrSz
                          }
        _ -> throwM $ ErrorIllegalTagWord ptr


getListPtr :: (Monad m, MonadThrow m, MonadQuota m)
    => Message m -> WordAddr -> EltSpec -> m (ListPtr m)
getListPtr msg addr (EltComposite len) = recurse $
    ListP True <$> getCompositeList msg addr (fromIntegral len)
getListPtr _ _ (EltNormal Sz0 count) = recurse $ return $ List0 $ List
    { length = return (fromIntegral count)
    , lookup = \i -> do
        checkBounds i (fromIntegral count)
        invoice 1
        return ()
    }
getListPtr msg (WordAt seg off) (EltNormal Sz64 count) = recurse $ do
    segment <- lookup msg seg
    return $ List64 $ slice segment off (fromIntegral count)
getListPtr msg addr@(WordAt seg off) (EltNormal SzPtr count) =
    recurse $ return $ ListP False $
        List { length = return $ fromIntegral count
             , lookup = \i -> do
                   checkBounds i (fromIntegral count)
                   getPtr msg (WordAt seg (off + i))
             }
