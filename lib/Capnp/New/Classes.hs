{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
module Capnp.New.Classes
    ( Parse(..)
    , Marshal(..)
    , Allocate(..)
    , EstimateAlloc(..)
    , TypedStruct(..)
    , IsWord(..)
    , newRoot
    , Parsed
    , structSizes
    , newFromRepr
    , newTypedStruct
    ) where

import           Capnp.Classes       (IsWord(..))
import           Capnp.Message       (Mutability(..))
import qualified Capnp.Message       as M
import qualified Capnp.Repr          as R
import qualified Capnp.Untyped       as U
import           Data.Int
import qualified Data.Vector         as V
import           Data.Word
import qualified GHC.Float           as F
import qualified Language.Haskell.TH as TH

-- | Capnp types that can be parsed into a more "natural" Haskell form.
--
-- * @t@ is the capnproto type.
-- * @p@ is the type of the parsed value.
class Parse t p | t -> p, p -> t where
    parse :: U.ReadCtx m 'Const => R.Raw 'Const t -> m p
    -- ^ Parse a value from a constant message
    encode :: U.RWCtx m s => M.Message ('Mut s) -> p -> m (R.Raw ('Mut s) t)
    -- ^ Encode a value into 'R.Raw' form, using the message as storage.

    default encode
        :: (U.RWCtx m s, EstimateAlloc t p, Marshal t p)
        => M.Message ('Mut s) -> p -> m (R.Raw ('Mut s) t)
    encode msg value = do
        raw <- new (estimateAlloc value) msg
        marshalInto raw value
        pure raw

-- | Types where the necessary allocation is inferrable from the parsed form.
--
-- ...this is most types.
class (Parse t p, Allocate t) => EstimateAlloc t p where
    estimateAlloc :: p -> AllocHint t

    default estimateAlloc :: AllocHint t ~ () => p -> AllocHint t
    estimateAlloc _ = ()

newFromRepr
    :: forall a r m s.
    ( R.Allocate r
    , 'R.Ptr ('Just r) ~ R.ReprFor a
    , U.RWCtx m s
    )
    => R.AllocHint r -> M.Message ('Mut s) -> m (R.Raw ('Mut s) a)
newFromRepr hint msg = R.Raw <$> R.alloc @r msg hint
    -- TODO(cleanup): new and alloc really ought to have the same argument order...


-- | Types which may be allocated directly inside a message.
class Allocate a where
    type AllocHint a
    -- ^ Extra information needed to allocate a value of this type, e.g. the
    -- length for a list. May be () if no extra info is needed.

    new :: U.RWCtx m s => AllocHint a -> M.Message ('Mut s) -> m (R.Raw ('Mut s) a)
    -- ^ @'new' hint msg@ allocates a new value of type @a@ inside @msg@.

    default new ::
        ( R.ReprFor a ~ 'R.Ptr ('Just pr)
        , R.Allocate pr
        , AllocHint a ~ R.AllocHint pr
        , U.RWCtx m s
        ) => AllocHint a -> M.Message ('Mut s) -> m (R.Raw ('Mut s) a)
    -- If the AllocHint is the same as that of the underlying Repr, then
    -- we can just use that implementation.
    new = newFromRepr @a

newTypedStruct :: forall a m s. (TypedStruct a, U.RWCtx m s) => M.Message ('Mut s) -> m (R.Raw ('Mut s) a)
newTypedStruct = newFromRepr (structSizes @a)

class Parse t p => Marshal t p where
    marshalInto :: U.RWCtx m s => R.Raw ('Mut s) t -> p -> m ()
    -- ^ Marshal a value into the pre-allocated object inside the message.
    --
    -- Note that caller must arrange for the object to be of the correct size.
    -- This is is not necessarily guaranteed; for example, list types must
    -- coordinate the length of the list.


structSizes :: forall a. TypedStruct a => (Word16, Word16)
structSizes = (numStructWords @a, numStructPtrs @a)

-- | Operations on typed structs.
class (R.IsStruct a, Allocate a, AllocHint a ~ ()) => TypedStruct a where
    -- Get the size of  the struct's word and pointer sections, respectively.
    numStructWords :: Word16
    numStructPtrs  :: Word16


newRoot
    :: forall a m s. (U.RWCtx m s, R.IsStruct a, Allocate a)
    => AllocHint a -> M.Message ('Mut s) -> m (R.Raw ('Mut s) a)
newRoot hint msg = do
    raw@(R.Raw struct) <- new @a hint msg
    U.setRoot struct
    pure raw


------ Parse instances for basic types -------

parseId :: (R.Untyped mut (R.ReprFor a) ~ a, U.ReadCtx m mut) => R.Raw mut a -> m a
parseId = pure . R.fromRaw

parseInt ::
    ( Integral a
    , Integral (R.Untyped mut (R.ReprFor a))
    , U.ReadCtx m mut
    ) => R.Raw mut a -> m a
parseInt = pure . fromIntegral . R.fromRaw

do
    let mkParseId ty =
            [d| instance Parse $ty $ty where
                    parse = parseId
                    encode _ = pure . R.Raw
            |]
        mkParseInt ty =
            [d| instance Parse $ty $ty where
                    parse = parseInt
                    encode _ = pure . R.Raw . fromIntegral
            |]
        nameTy name = pure (TH.ConT name)
        ids  = map nameTy [''Bool, ''Word8, ''Word16, ''Word32, ''Word64]
        ints = map nameTy [''Int8, ''Int16, ''Int32, ''Int64]

        merge :: [TH.Q [a]] -> TH.Q [a]
        merge xs = concat <$> sequenceA xs
    merge
        [ merge $ map mkParseId ids
        , merge $ map mkParseInt ints
        , mkParseId [t| () |]
        ]

instance Parse Float Float where
    parse = pure . F.castWord32ToFloat . R.fromRaw
    encode _ = pure . R.Raw . F.castFloatToWord32

instance Parse Double Double where
    parse = pure . F.castWord64ToDouble . R.fromRaw
    encode _ = pure . R.Raw . F.castDoubleToWord64

instance (R.FromElement (R.ReprFor a), Parse a ap) => Parse (R.List a) (V.Vector ap) where
    encode _ = undefined
    parse rawV =
        V.generateM (R.length rawV) $ \i ->
            R.index i rawV >>= parse

instance (Allocate l, AllocHint l ~ Int, Parse l (V.Vector a)) => EstimateAlloc l (V.Vector a) where
    estimateAlloc = V.length

-- | If @a@ is a capnproto type, then @Parsed a@ is an ADT representation of that
-- type. If this is defined for a type @a@ then there should also be an instance
-- @'Parse' a ('Parsed' a)@, but note that the converse is not true: if there is
-- an instance @'Parse' a b@, then @'Parsed' a@ needn't be defined, and @b@ can
-- be something else.
data family Parsed a
