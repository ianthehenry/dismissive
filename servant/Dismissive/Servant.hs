{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Dismissive.Servant where

import BasePrelude
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Dismissive.Api
import Servant

type Dismiss = DismissiveT (EitherT ServantErr IO) :~> EitherT ServantErr IO

getDismiss :: (MonadIO m, MonadBaseControl IO m, MonadBaseControl IO m', MonadLogger m', MonadIO m')
           => ConnectionString
           -> ((DismissiveT m :~> m) -> m' a)
           -> m' a
getDismiss connStr f =
  withDismissiveIO
  connStr
  -- GHC can't typecheck the pointfree equivalent (f . Nat), and I don't know why
  (\r -> f $ Nat r)
