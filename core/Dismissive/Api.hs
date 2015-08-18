{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Dismissive.Api (
  Dismissive,
  DismissiveT,
  DismissiveIO,
  runDismissiveT,
  withDismissiveIO,
  getUser,
  markSent,
  unsentReminders,
  insertReminder,
  Entity(..),
  keyShow,
  keyRead
) where

import BasePrelude hiding (insert, on)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Functor.Identity
import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.Esqueleto
import Database.Persist.Postgresql (withPostgresqlPool, ConnectionString, ConnectionPool, runSqlPersistMPool)
import Dismissive.Internal.Types
import Control.Monad.Logger
import Data.Time.Clock

keyShow :: ToBackendKey SqlBackend r => Key r -> Text
keyShow = Text.pack . show . fromSqlKey

keyRead :: ToBackendKey SqlBackend r => Text -> Key r
keyRead = toSqlKey . read . Text.unpack

newtype DismissiveT m a =
  DismissiveT { unDismissiveT :: ReaderT ConnectionPool m a
              } deriving ( Functor, Applicative, Monad
                         , MonadIO, MonadReader ConnectionPool
                         )
type DismissiveIO a = forall m. MonadIO m => DismissiveT m a
type Dismissive = DismissiveT Identity

runDismissiveT :: DismissiveT m a -> ConnectionPool -> m a
runDismissiveT d = runReaderT (unDismissiveT d)

withDismissiveIO :: ConnectionString -> DismissiveIO () -> IO ()
withDismissiveIO connStr handler =
  runStderrLoggingT $ withPostgresqlPool connStr 10 (runDismissiveT handler)

getUser :: EmailAddress -> DismissiveIO (Maybe (Entity User))
getUser email = run (getBy (UniqueEmail email))

unsentReminders :: DismissiveIO [(Entity Reminder, Entity User)]
unsentReminders = run $ do
  now <- liftIO getCurrentTime
  select $ from $ \(reminder `InnerJoin` user) -> do
    on (reminder ^. ReminderUserId ==. user ^. UserId)
    where_ (not_ $ reminder ^. ReminderSent)
    where_ (reminder ^. ReminderSendAt <=. val now)
    return (reminder, user)

markSent :: ReminderId -> DismissiveIO ()
markSent reminderId = run $ update $ \reminder -> do
  set reminder [ReminderSent =. val True]
  where_ (reminder ^. ReminderId ==. val reminderId)

run :: SqlPersistM a -> DismissiveIO a
run action = do
  pool <- ask
  liftIO $ runSqlPersistMPool action pool

insertReminder :: Reminder -> DismissiveIO ()
insertReminder reminder = (void . run) (insert reminder)
