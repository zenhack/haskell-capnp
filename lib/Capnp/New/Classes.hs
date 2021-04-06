{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
module Capnp.New.Classes
    ( Parse(..)
    ) where

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
