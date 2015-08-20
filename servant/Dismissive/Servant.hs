{-# LANGUAGE TypeOperators #-}

module Dismissive.Servant where

import BasePrelude
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Dismissive.Api
import Servant
import Network.Wai

type Dismiss = DismissiveT (EitherT ServantErr IO) :~> EitherT ServantErr IO

getDismiss :: Monad m => DismissiveT m Dismiss
getDismiss = do
  pool <- ask
  rng <- get
  return $ Nat (runDismissiveT rng pool)

makeApplication :: (MonadIO m, HasServer a) => Proxy a -> DismissiveT m (Server a) -> DismissiveT m Application
makeApplication api d = serve api <$> d
