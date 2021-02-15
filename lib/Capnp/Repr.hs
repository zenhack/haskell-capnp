{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
module Capnp.Repr
    ( Repr(..)
    , PtrRepr(..)
    , ListRepr(..)
    , NormalListRepr(..)
    , DataSz(..)
    , ElemRepr
    , ListReprFor
    , ReprFor
    , IsPtrRepr(..)
    , FromElement(..)
    , Untyped
    , UntypedData
    , Raw(..)
    , List
    , length
    , index
    ) where

import Prelude hiding (length)

import qualified Capnp.Classes       as C
import qualified Capnp.Errors        as E
import           Capnp.Message       (Mutability(..))
import qualified Capnp.Message       as M
import qualified Capnp.Untyped       as U
import           Control.Monad.Catch (MonadThrow(..))
import           Data.Int
import           Data.Kind           (Type)
import           Data.Word
import qualified Language.Haskell.TH as TH

-- | A 'Repr' describes a wire representation for a value. This is
-- mostly used at the type level (using DataKinds); types are
-- parametrized over representations.
data Repr
    = Ptr (Maybe PtrRepr)
    -- ^ Pointer type
    | Data DataSz
    -- ^ Non-pointer type.
    deriving(Show)

data PtrRepr
    = Cap
    | List (Maybe ListRepr)
    | Struct
    deriving(Show)

data ListRepr where
    ListNormal :: NormalListRepr -> ListRepr
    ListComposite :: ListRepr
    deriving(Show)

data NormalListRepr where
    ListData :: DataSz -> NormalListRepr
    ListPtr :: NormalListRepr
    deriving(Show)

-- | The size of a non-pointer type.
data DataSz = Sz0 | Sz1 | Sz8 | Sz16 | Sz32 | Sz64
    deriving(Show)

-- | @ElemRepr r@ is the representation of elements of lists with
-- representation @r@.
type family ElemRepr (rl :: ListRepr) :: Repr where
    ElemRepr 'ListComposite = 'Ptr ('Just 'Struct)
    ElemRepr ('ListNormal 'ListPtr) = 'Ptr 'Nothing
    ElemRepr ('ListNormal ('ListData sz)) = 'Data sz

-- | @ListReprFor e@ is the representation of lists with elements
-- whose representation is @e@.
type family ListReprFor (e :: Repr) :: ListRepr where
    ListReprFor ('Data sz) = 'ListNormal ('ListData sz)
    ListReprFor ('Ptr ('Just 'Struct)) = 'ListComposite
    ListReprFor ('Ptr a) = 'ListNormal 'ListPtr

type family ReprFor (a :: Type) :: Repr

type instance ReprFor () = 'Data 'Sz0
type instance ReprFor Bool = 'Data 'Sz1
type instance ReprFor Word8 = 'Data 'Sz8
type instance ReprFor Word16 = 'Data 'Sz16
type instance ReprFor Word32 = 'Data 'Sz32
type instance ReprFor Word64 = 'Data 'Sz64
type instance ReprFor Int8 = 'Data 'Sz8
type instance ReprFor Int16 = 'Data 'Sz16
type instance ReprFor Int32 = 'Data 'Sz32
type instance ReprFor Int64 = 'Data 'Sz64
type instance ReprFor Float = 'Data 'Sz32
type instance ReprFor Double = 'Data 'Sz64

type instance ReprFor (U.ListOf mut a) = ReprFor (List a)
type instance ReprFor (U.Struct mut) = 'Ptr ('Just 'Struct)
type instance ReprFor (U.Cap mut) = 'Ptr ('Just 'Cap)
type instance ReprFor (U.Ptr mut) = 'Ptr 'Nothing
type instance ReprFor (U.List mut) = 'Ptr ('Just ('List 'Nothing))

-- | @Untyped mut r@ is an untyped value with representation @r@ stored in
-- a message with mutability @mut@.
type family Untyped (mut :: Mutability) (r :: Repr) :: Type where
    Untyped mut ('Data sz) = UntypedData sz
    Untyped mut ('Ptr ptr) = UntypedPtr mut ptr

type family UntypedData (sz :: DataSz) :: Type where
    UntypedData 'Sz0 = ()
    UntypedData 'Sz1 = Bool
    UntypedData 'Sz8 = Word8
    UntypedData 'Sz16 = Word16
    UntypedData 'Sz32 = Word32
    UntypedData 'Sz64 = Word64

type family UntypedPtr (mut :: Mutability) (r :: Maybe PtrRepr) :: Type where
    UntypedPtr mut 'Nothing = Maybe (U.Ptr mut)
    UntypedPtr mut ('Just r) = UntypedSomePtr mut r

type family UntypedSomePtr (mut :: Mutability) (r :: PtrRepr) :: Type where
    UntypedSomePtr mut 'Struct = U.Struct mut
    UntypedSomePtr mut 'Cap = U.Cap mut
    UntypedSomePtr mut ('List r) = UntypedList mut r

type family UntypedList (mut :: Mutability) (r :: Maybe ListRepr) :: Type where
    UntypedList mut 'Nothing = U.List mut
    UntypedList mut ('Just r) = UntypedSomeList mut r

type family UntypedSomeList (mut :: Mutability) (r :: ListRepr) :: Type where
    UntypedSomeList mut r = U.ListOf mut (Untyped mut (ElemRepr r))

newtype Raw (mut :: Mutability) (a :: Type)
    = Raw { fromRaw :: Untyped mut (ReprFor a) }

data List a

type instance ReprFor (List a) = 'Ptr ('Just ('List ('Just (ListReprFor (ReprFor a)))))

length :: Raw mut (List a) -> Int
length (Raw l) = U.length l

index :: forall a m mut.
    ( U.ReadCtx m mut
    , FromElement (ReprFor a)
    ) => Int -> Raw mut (List a) -> m (Raw mut a)
index i (Raw l) =
    Raw <$> (U.index i l >>= fromElement @(ReprFor a) @m @mut (U.message l))

-- | 'FromElement' supports converting a value of representation
-- @'ElemRepr' ('ListReprFor' r)@ into a value of representation @r@.
--
-- At a glance, you might expect this to just be a no-op, but it is actually
-- *not* always the case that @'ElemRepr' ('ListReprFor' r) ~ r@; in the
-- case of pointer type, @'ListReprFor' r@ can contain arbitrary pointers,
-- so information is lost, and it is possible for the list to contain pointers
-- of the incorrect type. In this case, 'fromElement' will throw an error.
class FromElement (r :: Repr) where
    fromElement
        :: forall m mut. U.ReadCtx m mut
        => M.Message mut
        -> Untyped mut (ElemRepr (ListReprFor r))
        -> m (Untyped mut r)

instance FromElement ('Data sz) where
    fromElement _ = pure
instance FromElement ('Ptr ('Just 'Struct)) where
    fromElement _ = pure
instance FromElement ('Ptr 'Nothing) where
    fromElement _ = pure
instance FromElement ('Ptr ('Just 'Cap)) where
    fromElement = rFromPtr @('Just 'Cap)

instance (ReprFor a ~ 'Ptr ('Just 'Struct)) => C.ToStruct mut (Raw mut a) where
    toStruct = fromRaw
instance (ReprFor a ~ 'Ptr ('Just 'Struct)) => C.FromStruct mut (Raw mut a) where
    fromStruct = pure . Raw

instance U.HasMessage (Untyped mut (ReprFor a)) mut => U.HasMessage (Raw mut a) mut where
    message (Raw r) = U.message r
instance U.MessageDefault (Untyped mut (ReprFor a)) mut => U.MessageDefault (Raw mut a) mut where
    messageDefault msg = Raw <$> U.messageDefault msg

class IsPtrRepr (r :: Maybe PtrRepr) where
    rToPtr :: M.Message mut -> Untyped mut ('Ptr r) -> Maybe (U.Ptr mut)
    rFromPtr :: U.ReadCtx m mut => M.Message mut -> Maybe (U.Ptr mut) -> m (Untyped mut ('Ptr r))

instance IsPtrRepr 'Nothing where
    rToPtr _ p = p
    rFromPtr _ p = pure p

instance IsPtrRepr ('Just 'Struct) where
    rToPtr _ s = Just (U.PtrStruct s)
    rFromPtr msg Nothing              = U.messageDefault msg
    rFromPtr _ (Just (U.PtrStruct s)) = pure s
    rFromPtr _ _                      = expected "pointer to struct"
instance IsPtrRepr ('Just 'Cap) where
    rToPtr _ c = Just (U.PtrCap c)
    rFromPtr _ Nothing             = expected "pointer to capability"
    rFromPtr _ (Just (U.PtrCap c)) = pure c
    rFromPtr _ _                   = expected "pointer to capability"
instance IsPtrRepr ('Just ('List 'Nothing)) where
    rToPtr _ l = Just (U.PtrList l)
    rFromPtr _ Nothing              = expected "pointer to list"
    rFromPtr _ (Just (U.PtrList l)) = pure l
    rFromPtr _ (Just _)             = expected "pointer to list"
instance IsListPtrRepr r => IsPtrRepr ('Just ('List ('Just r))) where
    rToPtr _ l = Just (U.PtrList (rToList @r l))
    rFromPtr msg Nothing            = rFromListMsg @r msg
    rFromPtr _ (Just (U.PtrList l)) = rFromList @r l
    rFromPtr _ (Just _)             = expected "pointer to list"

class IsListPtrRepr (r :: ListRepr) where
    rToList :: UntypedSomeList mut r -> U.List mut
    rFromList :: U.ReadCtx m mut => U.List mut -> m (UntypedSomeList mut r)
    rFromListMsg :: U.ReadCtx m mut => M.Message mut -> m (UntypedSomeList mut r)

-- helper function for throwing SchemaViolationError "expected ..."
expected :: MonadThrow m => String -> m a
expected msg = throwM $ E.SchemaViolationError $ "expected " ++ msg

do
    let mkIsListPtrRepr (r, listC, str) =
            [d| instance IsListPtrRepr $r where
                    rToList = $(pure $ TH.ConE listC)
                    rFromList $(pure $ TH.ConP listC [TH.VarP (TH.mkName "l")]) = pure l
                    rFromList _ = expected $(pure $ TH.LitE $ TH.StringL $ "pointer to " ++ str)
                    rFromListMsg = U.messageDefault
            |]
    concat <$> traverse mkIsListPtrRepr
        [ ( [t| 'ListNormal ('ListData 'Sz0) |]
          , 'U.List0
          , "List(Void)"
          )
        , ( [t| 'ListNormal ('ListData 'Sz1) |]
          , 'U.List1
          , "List(Bool)"
          )
        , ( [t| 'ListNormal ('ListData 'Sz8) |]
          , 'U.List8
          , "List(UInt8)"
          )
        , ( [t| 'ListNormal ('ListData 'Sz16) |]
          , 'U.List16
          , "List(UInt16)"
          )
        , ( [t| 'ListNormal ('ListData 'Sz32) |]
          , 'U.List32
          , "List(UInt32)"
          )
        , ( [t| 'ListNormal ('ListData 'Sz64) |]
          , 'U.List64
          , "List(UInt64)"
          )
        , ( [t| 'ListNormal 'ListPtr |]
          , 'U.ListPtr
          , "List(AnyPointer)"
          )
        , ( [t| 'ListComposite |]
          , 'U.ListStruct
          , "composite list"
          )
        ]

instance (IsPtrRepr r, ReprFor a ~ 'Ptr r) => C.ToPtr s (Raw ('Mut s) a) where
    toPtr msg (Raw p) = pure $ rToPtr @r msg p
instance (IsPtrRepr r, ReprFor a ~ 'Ptr r) => C.FromPtr mut (Raw mut a) where
    fromPtr msg p = Raw <$> rFromPtr @r msg p
