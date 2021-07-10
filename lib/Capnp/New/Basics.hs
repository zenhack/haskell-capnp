{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Capnp.New.Basics where

import qualified Capnp.Errors        as E
import qualified Capnp.Message       as M
import qualified Capnp.New.Classes   as C
import qualified Capnp.Repr          as R
import qualified Capnp.Untyped       as U
import           Control.Monad       (when)
import           Control.Monad.Catch (throwM)
import qualified Data.ByteString     as BS
import           Data.Foldable       (for_)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import qualified Data.Vector         as V
import           Data.Word

data Text
data Data
data AnyPointer
data AnyList
data AnyStruct
data Capability

type instance R.ReprFor Data = R.ReprFor (R.List Word8)
type instance R.ReprFor Text = R.ReprFor (R.List Word8)
type instance R.ReprFor AnyPointer = 'R.Ptr 'Nothing
type instance R.ReprFor AnyList = 'R.Ptr ('Just ('R.List 'Nothing))
type instance R.ReprFor AnyStruct = 'R.Ptr ('Just 'R.Struct)
type instance R.ReprFor Capability = 'R.Ptr ('Just 'R.Cap)

data instance C.Parsed AnyPointer
    = PtrNull
    | PtrStruct (C.Parsed AnyStruct)
    | PtrList (C.Parsed AnyList)
    | PtrCap M.Client
    deriving(Show, Eq)

instance C.Parse AnyPointer (C.Parsed AnyPointer) where
    parse (R.Raw ptr) = case ptr of
        Nothing                   -> pure PtrNull
        Just (U.PtrCap cap)       -> PtrCap <$> C.parse (R.Raw cap)
        Just (U.PtrList list)     -> PtrList <$> C.parse (R.Raw list)
        Just (U.PtrStruct struct) -> PtrStruct <$> C.parse (R.Raw struct)

    encode msg value = R.Raw <$> case value of
        PtrNull       -> pure Nothing
        PtrCap cap    -> Just . U.PtrCap . R.fromRaw <$> C.encode msg cap
        PtrList list -> Just . U.PtrList . R.fromRaw <$> C.encode msg list
        PtrStruct struct -> Just . U.PtrStruct . R.fromRaw <$> C.encode msg struct

instance C.AllocateList AnyPointer where
    type ListAllocHint AnyPointer = Int

instance C.EstimateListAlloc AnyPointer (C.Parsed AnyPointer)

data instance C.Parsed AnyStruct = Struct
    { structData :: V.Vector Word64
    , structPtrs :: V.Vector (C.Parsed AnyPointer)
    }
    deriving(Show, Eq)

instance C.Parse AnyStruct (C.Parsed AnyStruct) where
    parse (R.Raw s) = Struct
        <$> V.generateM
                (fromIntegral $ U.structWordCount s)
                (`U.getData` s)
        <*> V.generateM
                (fromIntegral $ U.structPtrCount s)
                (\i -> U.getPtr i s >>= C.parse . R.Raw)

instance C.AllocateList AnyStruct where
    type ListAllocHint AnyStruct = (Int, R.AllocHint 'R.Struct)

instance C.EstimateListAlloc AnyStruct (C.Parsed AnyStruct) where
    estimateListAlloc structs =
        let len = V.length structs
            nWords = maximum $ map (V.length . structData) $ V.toList structs
            nPtrs = maximum $ map (V.length . structPtrs) $ V.toList structs
        in
        (len, (fromIntegral nWords, fromIntegral nPtrs))

instance C.EstimateAlloc AnyStruct (C.Parsed AnyStruct) where
    estimateAlloc s =
        ( fromIntegral $ V.length $ structData s
        , fromIntegral $ V.length $ structPtrs s
        )

instance C.Marshal AnyStruct (C.Parsed AnyStruct) where
    marshalInto (R.Raw raw) s = do
        V.iforM_ (structData s) $ \i value -> do
            U.setData value i raw
        V.iforM_ (structPtrs s) $ \i value -> do
            R.Raw ptr <- C.encode (U.message raw) value
            U.setPtr ptr i raw

-- TODO(cleanup): It would be nice if we could reuse Capnp.Repr.Parsed.Parsed
-- here, but that would cause a circular import dependency.
type ParsedList a = V.Vector a

data instance C.Parsed AnyList
    = ListPtr (ParsedList (C.Parsed AnyPointer))
    | ListStruct (ParsedList (C.Parsed AnyStruct))
    | List0 (ParsedList ())
    | List1 (ParsedList Bool)
    | List8 (ParsedList Word8)
    | List16 (ParsedList Word16)
    | List32 (ParsedList Word32)
    | List64 (ParsedList Word64)
    deriving(Show, Eq)

instance C.Parse AnyList (C.Parsed AnyList) where
    parse (R.Raw list) = case list of
        U.List0 l      -> List0 <$> C.parse (R.Raw l)
        U.List1 l      -> List1 <$> C.parse (R.Raw l)
        U.List8 l      -> List8 <$> C.parse (R.Raw l)
        U.List16 l     -> List16 <$> C.parse (R.Raw l)
        U.List32 l     -> List32 <$> C.parse (R.Raw l)
        U.List64 l     -> List64 <$> C.parse (R.Raw l)
        U.ListPtr l    -> ListPtr <$> C.parse (R.Raw l)
        U.ListStruct l -> ListStruct <$> C.parse (R.Raw l)

    encode msg list = R.Raw <$> case list of
        List0 l      -> U.List0 . R.fromRaw <$> C.encode msg l
        List1 l      -> U.List1 . R.fromRaw <$> C.encode msg l
        List8 l      -> U.List8 . R.fromRaw <$> C.encode msg l
        List16 l     -> U.List16 . R.fromRaw <$> C.encode msg l
        List32 l     -> U.List32 . R.fromRaw <$> C.encode msg l
        List64 l     -> U.List64 . R.fromRaw <$> C.encode msg l
        ListPtr l    -> U.ListPtr . R.fromRaw <$> C.encode msg l
        ListStruct l -> U.ListStruct . R.fromRaw <$> C.encode msg l

instance C.Parse Capability M.Client where
    parse (R.Raw cap) = U.getClient cap
    encode msg client = R.Raw <$> U.appendCap msg client

instance C.Allocate Text where
    type AllocHint Text = Int
    new len msg = R.Raw <$> U.allocList8 msg (len + 1)

instance C.AllocateList Text where
    type ListAllocHint Text = Int
instance C.EstimateListAlloc Text T.Text

instance C.Parse Text T.Text where
    parse (R.Raw list) =
        let len = U.length list in
        if len == 0 then
            -- We are somewhat lenient here; technically this is invalid, as there is
            -- no null terminator (see logic below, which is dead code because of
            -- this check. But to avoid this we really need to expose nullability
            -- in the API, so for now we just fudge it.
            pure ""
        else (do
            when (len == 0) $ throwM $ E.SchemaViolationError
                "Text is not NUL-terminated (list of bytes has length 0)"
            lastByte <- U.index (len - 1) list
            when (lastByte /= 0) $ throwM $ E.SchemaViolationError $
                "Text is not NUL-terminated (last byte is " ++ show lastByte ++ ")"
            bytes <- BS.take (len - 1) <$> U.rawBytes list
            case TE.decodeUtf8' bytes of
                Left e  -> throwM $ E.InvalidUtf8Error e
                Right v -> pure v)
    encode msg value = do
        let bytes = TE.encodeUtf8 value
        raw@(R.Raw untyped)  <- C.new @Text (BS.length bytes) msg
        C.marshalInto @Data (R.Raw untyped) bytes
        pure raw

-- Instances for Data
instance C.Parse Data BS.ByteString where
    parse = U.rawBytes . R.fromRaw

instance C.EstimateAlloc Data BS.ByteString where
    estimateAlloc = BS.length


instance C.Allocate Data where
    type AllocHint Data = Int
    new len msg = R.Raw <$> U.allocList8 msg len

instance C.Marshal Data BS.ByteString where
    marshalInto (R.Raw list) bytes =
        for_ [0..BS.length bytes - 1] $ \i ->
            U.setIndex (BS.index bytes i) i list

-- Instances for AnyStruct
instance C.Allocate AnyStruct where
    type AllocHint AnyStruct = (Word16, Word16)
    new (nWords, nPtrs) msg = R.Raw <$> U.allocStruct msg nWords nPtrs
