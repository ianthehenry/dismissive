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
  unsentMessages,
  insertMessage,
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

unsentMessages :: DismissiveIO [(Entity Message, Entity User)]
unsentMessages = run $ do
  now <- liftIO getCurrentTime
  select $ from $ \(message `InnerJoin` user) -> do
    on (message ^. MessageUserId ==. user ^. UserId)
    where_ (not_ $ message ^. MessageSent)
    where_ (message ^. MessageSendAt <=. val now)
    return (message, user)

markSent :: MessageId -> DismissiveIO ()
markSent messageId = run $ update $ \message -> do
  set message [MessageSent =. val True]
  where_ (message ^. MessageId ==. val messageId)

run :: SqlPersistM a -> DismissiveIO a
run action = do
  pool <- ask
  liftIO $ runSqlPersistMPool action pool

insertMessage :: Message -> DismissiveIO ()
insertMessage message = (void . run) (insert message)
