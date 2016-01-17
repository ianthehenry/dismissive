{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Dismissive.Internal.Stack where

import BasePrelude
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Trans.Control
import Crypto.Random.DRBG
import Database.Persist.Sql (SqlPersistT)

newtype DismissiveT m a =
  DismissiveT { unDismissiveT :: StateT HashDRBG (SqlPersistT m) a
              } deriving ( Functor, Applicative, Monad
                         , MonadIO, MonadState HashDRBG
                         , MonadError e
                         )

deriving instance (MonadBase b m) => MonadBase b (DismissiveT m)

instance MonadReader r m => MonadReader r (DismissiveT m) where
  ask = lift ask
  local = mapDismissT . local
  reader = lift . reader

mapDismissT :: (m (a, HashDRBG) -> n (b, HashDRBG)) -> DismissiveT m a -> DismissiveT n b
mapDismissT f = DismissiveT . (mapStateT . mapReaderT) f . unDismissiveT

instance MonadTrans DismissiveT where
  lift = DismissiveT . lift . lift

instance MonadTransControl DismissiveT where
    type StT DismissiveT a = StT SqlPersistT (StT (StateT HashDRBG) a)
    liftWith f = DismissiveT $ liftWith $ \run ->
                               liftWith $ \run' ->
                                 f (run' . run . unDismissiveT)
    restoreT = DismissiveT . restoreT . restoreT

instance MonadBaseControl b m => MonadBaseControl b (DismissiveT m) where
  type StM (DismissiveT m) a = ComposeSt DismissiveT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

type DismissiveIO a = forall m. MonadIO m => DismissiveT m a
