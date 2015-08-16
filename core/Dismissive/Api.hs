{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dismissive.Api (
  Dismissive,
  DismissiveT,
  DismissiveIO,
  runDismissiveT,
  withDismissiveIO,
  getUser,
  insertMessage,
  Entity(..)
) where

import BasePrelude hiding (insert)
import Data.Text (Text)
import Data.Functor.Identity
import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.Persist
import Database.Persist.Postgresql
import Dismissive.Types
import Control.Monad.Logger

newtype DismissiveT m a =
  DismissiveT { unDismissiveT :: ReaderT ConnectionPool m a
              } deriving (Monad, MonadIO, MonadReader ConnectionPool, Functor, Applicative)
type DismissiveIO a = forall m. MonadIO m => DismissiveT m a
type Dismissive = DismissiveT Identity

runDismissiveT :: DismissiveT m a -> ConnectionPool -> m a
runDismissiveT d = runReaderT (unDismissiveT d)

withDismissiveIO :: ConnectionString -> DismissiveIO () -> IO ()
withDismissiveIO connStr handler =
  runStderrLoggingT $ withPostgresqlPool connStr 10 (runDismissiveT handler)

getUser :: EmailAddress -> DismissiveIO (Maybe (Entity User))
getUser email = do
  pool <- ask
  liftIO $ runSqlPersistMPool (getBy (UniqueEmail email)) pool

insertMessage :: Message -> DismissiveIO ()
insertMessage message = do
  pool <- ask
  liftIO $ runSqlPersistMPool (insert message) pool
  return ()
