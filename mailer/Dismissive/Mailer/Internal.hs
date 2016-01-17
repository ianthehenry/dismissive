{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Dismissive.Mailer.Internal where

import BasePrelude
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Control
import Control.Monad.Base
import Data.Text (Text)

type EmailAddress = Text

data MailerConf =
  MailerConf { mailerKey :: Text
             , mailerDomain :: Text
             }

newtype MailerT m a =
  MailerT { unMailerT :: ReaderT MailerConf m a
          } deriving ( Functor, Applicative, Monad
                     , MonadIO, MonadReader MailerConf
                     , MonadError e
                     )

deriving instance (MonadBase b m) => MonadBase b (MailerT m)

runMailerT :: Monad m => MailerConf -> MailerT m a -> m a
runMailerT conf mailer = runReaderT (unMailerT mailer) conf

instance MonadTrans MailerT where
  lift = MailerT . lift

instance MonadTransControl MailerT where
  type StT MailerT a = StT (ReaderT MailerConf) a
  liftWith = defaultLiftWith MailerT unMailerT
  restoreT = defaultRestoreT MailerT

instance MonadBaseControl b m => MonadBaseControl b (MailerT m) where
  type StM (MailerT m) a = ComposeSt MailerT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM
