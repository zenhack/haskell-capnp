{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Capnp.Untyped.Mutable
    ( Ptr(..), List(..), Struct, ListOf
    ) where

import Data.Word

import qualified Data.Capnp.Message.Mutable as M

data Ptr s
    = PtrCap (M.Message s) !Word32
    | PtrList (List s)
    | PtrStruct (Struct s)

data List s
    = List0 (ListOf s ())
    | List1 (ListOf s Bool)
    | List8 (ListOf s Word8)
    | List16 (ListOf s Word16)
    | List32 (ListOf s Word32)
    | List64 (ListOf s Word64)
    | ListPtr (ListOf s (Maybe (Ptr s)))
    | ListStruct (ListOf s (Struct s))

data Struct s = Struct
    (M.Message s)
    !WordAddr
    !Word16
    !Word16

data ListOf s a where
    ListOfVoid :: M.Message s -> !Int -> ListOf s ()
    ListOfStruct :: Struct s -> !Int -> ListOf s (Struct s)
    ListOfBool :: !NormalList s -> ListOf s Bool
    ListOfWord8  :: !NormalList s -> ListOf s Word8
    ListOfWord16 :: !NormalList s -> ListOf s Word16
    ListOfWord32 :: !NormalList s -> ListOf s Word32
    ListOfWord64 :: !NormalList s -> ListOf s Word64
    ListOfPtr    :: !NormalList s -> ListOf s (Maybe (Ptr s))

data NormalList = NormalList
    { nMsg  :: M.Message s
    , nAddr :: WordAddr
    , nLen  :: !Int
    }


dataSection :: Struct s -> ListOf s Word64
dataSection (Struct msg addr dataSz _) =
    ListOfWord64 $ NormalList msg addr (fromIntegral dataSz)

ptrSection :: Struct s -> ListOf s (Maybe (Ptr s))
ptrSection (Struct msg addr@WordAt{..} dataSz ptrSz) =
    ListOfPtr $ NormalList
        msg
        addr { wordIndex = wordIndex + fromIntegral dataSz }
        (fromIntegral ptrSz)
