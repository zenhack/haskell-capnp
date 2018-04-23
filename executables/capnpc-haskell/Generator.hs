{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Generator
    ( GenT
    , evalGenT
    , emit
    )
  where

import Namespace

import Control.Monad.Catch       (MonadThrow(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Writer      (WriterT, execWriterT, tell)
import Data.DList                (DList)

import qualified Data.DList          as DList
import qualified Data.Map.Strict     as M
import qualified Language.Haskell.TH as TH

import Data.CapNProto.Errors         (ThrowError(..))
import Data.CapNProto.TraversalLimit (Limit(invoice))

-- | A monad transformer that adds the ability to emit template haskell
-- declarations, scoped to specific modules. See 'emit'.
newtype GenT m a = GenT ((WriterT GenAcc m) a)
    deriving(Functor, Applicative, Monad)

instance MonadTrans GenT where
    lift = GenT . lift

instance (Monad m, Limit m) => Limit (GenT m) where
    invoice = lift . invoice

instance (Monad m, ThrowError m) => ThrowError (GenT m) where
    throwError = lift . throwError

-- | @emit ns@ Emits a declaration to be added to the module named by @ns@.
emit :: Monad m => NS -> TH.DecsQ -> GenT m ()
emit k v = GenT $ tell $ GenAcc $ M.singleton k $ fmap DList.fromList v

evalGenT :: Monad m => GenT m a -> m (M.Map NS TH.DecsQ)
evalGenT (GenT m) = do
    GenAcc acc <- execWriterT m
    return $ M.map (fmap DList.toList) acc

-- | Internal state for a 'GenT'; we maintain a mapping from module names
-- to the declarations to be built for those modules. We define a Monoid
-- instance, so we can use WriterT internally.
newtype GenAcc = GenAcc (M.Map NS (TH.Q (DList TH.Dec)))

instance Monoid GenAcc where
    mempty = GenAcc M.empty
    mappend (GenAcc l) (GenAcc r) = GenAcc $ M.unionWith mergeTHs l r
      where
        mergeTHs l r = mappend <$> l <*> r

instance MonadThrow m => MonadThrow (GenT m) where
    throwM = lift . throwM
