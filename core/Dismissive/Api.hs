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
  addReminder,
  Entity(..),
  TokenError(..),
  keyShow,
  keyRead
) where

import BasePrelude hiding (insert, on, left)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Functor.Identity
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Reader
import Database.Esqueleto
import Database.Persist.Postgresql (withPostgresqlPool, ConnectionString, ConnectionPool, runSqlPersistMPool)
import Dismissive.Internal.Types
import Dismissive.Types
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

data TokenError = TokenNotFound
                | TokenLacksPermissions [Permission]
                  deriving (Eq, Show)

liftMaybe :: Monad m => a -> Maybe b -> EitherT a m b
liftMaybe err Nothing = left err
liftMaybe _ (Just x)  = pure x

ensure :: Monad m => Bool -> a -> EitherT a m ()
ensure True  _   = return ()
ensure False err = left err

insertReminder :: Reminder -> DismissiveIO ()
insertReminder reminder = (void . run) (insert reminder)

addReminder :: Token -> UTCTime -> Text -> DismissiveIO (Either TokenError ())
addReminder token sendAt text = runEitherT $ do
  (entityVal -> tokenRow) <- liftMaybe TokenNotFound =<< lift (run query)
  ensure (tokenRowCreate tokenRow) (TokenLacksPermissions [PermissionCreate])
  let reminder = Reminder text sendAt False (tokenRowUserId tokenRow)
  lift (insertReminder reminder)
  where query = getBy (UniqueToken token)
