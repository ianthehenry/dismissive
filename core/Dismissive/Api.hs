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
  createToken,
  Entity(..),
  TokenError(..),
  keyShow,
  keyRead
) where

import BasePrelude hiding (insert, on, left)
import Crypto.Random.DRBG
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Functor.Identity
import Data.ByteString (ByteString)
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Reader
import Control.Monad.State
import Database.Esqueleto hiding (isNothing, get)
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
  DismissiveT { unDismissiveT :: StateT HashDRBG (ReaderT ConnectionPool m) a
              } deriving ( Functor, Applicative, Monad
                         , MonadIO, MonadReader ConnectionPool
                         , MonadState HashDRBG
                         )
type DismissiveIO a = forall m. MonadIO m => DismissiveT m a
type Dismissive = DismissiveT Identity

runDismissiveT :: Monad m => HashDRBG -> ConnectionPool -> DismissiveT m a -> m a
runDismissiveT rng pool d = runReaderT (evalStateT (unDismissiveT d) rng) pool

withDismissiveIO :: ConnectionString -> DismissiveIO () -> IO ()
withDismissiveIO connStr handler = do
  rng <- newGenIO :: IO HashDRBG
  runStderrLoggingT $ withPostgresqlPool connStr 10 (\pool -> runDismissiveT rng pool handler)

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

-- Note: this does not attempt to do any checking
-- based on user id. That is to say, this only
-- checks that a token has read permissions. It
-- does not and cannot ensure that you only
-- read your own reminders.
lacking :: [Permission] -> TokenRow -> [Permission]
lacking permissions tokenRow = filter (not . hasPermission) permissions
  where
    hasPermission PermissionReadAll = isGlobal && canRead
    hasPermission PermissionEditAll = isGlobal && canEdit
    hasPermission PermissionCreate = isGlobal && canCreate
    hasPermission (PermissionEdit x) = reminderId == (Just x) && canEdit
    isGlobal = isNothing reminderId
    reminderId = tokenRowReminderId tokenRow
    canRead = tokenRowRead tokenRow
    canCreate = tokenRowCreate tokenRow
    canEdit = False

authorized :: [Permission] -> Token -> (TokenRow -> DismissiveIO a) -> DismissiveIO (Either TokenError a)
authorized permissions token cont = runEitherT $ do
  (entityVal -> tokenRow) <- liftMaybe TokenNotFound =<< lift (run query)
  let permissionsLacking = lacking permissions tokenRow
  ensure (null permissionsLacking) (TokenLacksPermissions permissions)
  lift (cont tokenRow)
  where query = getBy (UniqueToken token)

addReminder :: Token -> UTCTime -> Text -> DismissiveIO (Either TokenError ())
addReminder token sendAt text = authorized [PermissionCreate] token $ \tokenRow -> do
  let reminder = Reminder text sendAt False (tokenRowUserId tokenRow)
  insertReminder reminder

data PermSet =
  PermSet { permEdit :: Bool
          , permCreate :: Bool
          , permRead :: Bool
          , permReminder :: Maybe ReminderId
          }

fromPerms :: [Permission] -> Either () PermSet
fromPerms = foldM step (PermSet False False False Nothing)
  where
    step :: PermSet -> Permission -> Either () PermSet
    step p@(PermSet { permReminder = Nothing, permEdit   = False }) PermissionEditAll  = Right $ p { permEdit = True }
    step p@(PermSet { permReminder = Nothing, permEdit   = False }) (PermissionEdit x) = Right $ p { permEdit = True, permReminder = Just x }
    step p@(PermSet { permReminder = Nothing, permRead   = False }) PermissionReadAll  = Right $ p { permRead = True }
    step p@(PermSet { permReminder = Nothing, permCreate = False }) PermissionCreate   = Right $ p { permCreate = True }
    step _                                                          _                  = Left ()

liftEither :: Monad m => (a -> c) -> Either a b -> EitherT c m b
liftEither f (Left x) = left (f x)
liftEither _ (Right x) = return x

fromRight :: b -> Either a b -> b
fromRight _ (Right x) = x
fromRight x _ = x

maybeState :: MonadState s m => (s -> Either e (a, s)) -> m (Either e a)
maybeState f = do
  val <- f <$> get
  case val of
    Left e -> return (Left e)
    Right (a, s) -> put s $> return a

secureRandom :: Int -> DismissiveIO (Either GenError ByteString)
secureRandom byteCount = maybeState (genBytes byteCount)

data TokenCreateError = InvalidPermissionSet
                      | RandomnessProblem

createToken :: [Permission] -> UserId -> DismissiveIO (Either TokenCreateError Token)
createToken permissions userId = runEitherT $ do
  permSet <- liftEither (const InvalidPermissionSet) (fromPerms permissions)
  token <- liftEither (const RandomnessProblem) =<< lift (secureRandom 16)
  let PermSet { permReminder, permRead, permCreate } = permSet
  let row = TokenRow token permRead permCreate userId permReminder
  lift (insertToken row)
  return token

insertToken :: TokenRow -> DismissiveIO (Key TokenRow)
insertToken = run . insert
