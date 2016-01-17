{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Dismissive.Servant where

import BasePrelude
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Except
import Control.Monad.Trans.Control
import Dismissive.Api
import Dismissive.Mailer
import Servant

type Dismiss = DismissiveT (MailerT (ExceptT ServantErr IO)) :~> EitherT ServantErr IO

getDismiss :: (MonadBaseControl IO m', MonadLogger m', MonadIO m')
           => ConnectionString
           -> MailerConf
           -> (Dismiss -> m' a)
           -> m' a
getDismiss connStr mailerConf f =
  withDismissiveIO connStr $ \runDismissiveT ->
    f $ Nat (EitherT . runExceptT . runMailerT mailerConf . runDismissiveT)
