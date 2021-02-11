{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
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
class Parse a where
    type Parsed a
    -- ^ The type of a parsed value
    parseConst :: U.ReadCtx m 'Const => R.Raw 'Const a -> m (Parsed a)
    -- ^ Parse a value from a constant message
    parseMut :: U.RWCtx m s => R.Raw ('Mut s) a -> m (Parsed a)
    -- ^ Parse a value from a mutable message
    encode :: U.RWCtx m s => M.Message ('Mut s) -> Parsed a -> m (R.Raw ('Mut s) a)
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
            [d| instance Parse $ty where
                    type Parsed $ty = $ty
                    parseConst = parseId
                    parseMut = parseId
                    encode _ = pure . R.Raw
            |]
        mkParseInt ty =
            [d| instance Parse $ty where
                    type Parsed $ty = $ty
                    parseConst = parseInt
                    parseMut = parseInt
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

instance Parse Float where
    type Parsed Float = Float
    parseConst = pure . F.castWord32ToFloat . R.fromRaw
    parseMut = pure . F.castWord32ToFloat . R.fromRaw
    encode _ = pure . R.Raw . F.castFloatToWord32

instance Parse Double where
    type Parsed Double = Double
    parseConst = pure . F.castWord64ToDouble . R.fromRaw
    parseMut = pure . F.castWord64ToDouble . R.fromRaw
    encode _ = pure . R.Raw . F.castDoubleToWord64

instance (R.FromElement (R.ReprFor a), Parse a) => Parse (R.List a) where
    type Parsed (R.List a) = V.Vector (Parsed a)
    parseConst = parseList parseConst
    parseMut = parseList parseMut
    encode _ = undefined

parseList ::
    ( U.ReadCtx m mut
    , R.FromElement (R.ReprFor a)
    ) => (R.Raw mut a -> m (Parsed a)) -> R.Raw mut (R.List a) -> m (Parsed (R.List a))
parseList parseElt rawV =
    V.generateM (R.length rawV) $ \i ->
        R.index i rawV >>= parseElt
