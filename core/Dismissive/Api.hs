{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Dismissive.Api (
  DismissiveT,
  DismissiveIO,
  ConnectionString,
  withDismissiveIO,
  createAccount,
  getUser,
  markSent,
  unsentReminders,
  addReminder,
  createToken,
  Entity(..),
  TokenError(..),
  AccountCreateError(..),
  keyShow,
  keyRead
) where

import BasePrelude hiding (insert, insertBy, on, left)
import Crypto.Random.DRBG
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Functor.Identity
import Data.ByteString (ByteString)
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Logger (MonadLogger)
import Database.Esqueleto hiding (isNothing, get)
import Database.Persist.Postgresql (withPostgresqlPool, ConnectionString, ConnectionPool, runSqlPersistMPool)
import Dismissive.Internal.Types
import Dismissive.Types
import Dismissive.Internal.Stack
import Control.Monad.Logger
import Data.Time.Clock

keyShow :: ToBackendKey SqlBackend r => Key r -> Text
keyShow = Text.pack . show . fromSqlKey

keyRead :: ToBackendKey SqlBackend r => Text -> Key r
keyRead = toSqlKey . read . Text.unpack

withDismissiveIO :: (MonadIO m, MonadBaseControl IO m, MonadBaseControl IO m', MonadLogger m', MonadIO m')
                 => ConnectionString
                 -> ((forall a. DismissiveT m a -> m a) -> m' b)
                 -> m' b
withDismissiveIO connStr f = withPostgresqlPool connStr 10 $ \pool ->
  let unwrap action = do
        rng <- liftIO (newGenIO :: IO HashDRBG)
        let sqlPart = evalStateT (unDismissiveT action) rng
        runSqlPool sqlPart pool
  in f unwrap

getUser :: EmailAddress -> DismissiveIO (Maybe (Entity User))
getUser email = liftQuery (getBy (UniqueEmail email))

unsentReminders :: DismissiveIO [(Entity Reminder, Entity User)]
unsentReminders = liftQuery $ do
  now <- liftIO getCurrentTime
  select $ from $ \(reminder `InnerJoin` user) -> do
    on (reminder ^. ReminderUserId ==. user ^. UserId)
    where_ (not_ $ reminder ^. ReminderSent)
    where_ (reminder ^. ReminderSendAt <=. val now)
    return (reminder, user)

liftQuery :: Monad m => SqlPersistT m a -> DismissiveT m a
liftQuery = DismissiveT . lift

markSent :: ReminderId -> DismissiveIO ()
markSent reminderId = liftQuery $ update $ \reminder -> do
  set reminder [ReminderSent =. val True]
  where_ (reminder ^. ReminderId ==. val reminderId)

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
insertReminder reminder = (void . liftQuery) (insert reminder)

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
    canEdit = tokenRowEdit tokenRow

authorized :: [Permission] -> Token -> (TokenRow -> DismissiveIO a) -> DismissiveIO (Either TokenError a)
authorized permissions token cont = runEitherT $ do
  (entityVal -> tokenRow) <- liftMaybe TokenNotFound =<< lift query
  let permissionsLacking = lacking permissions tokenRow
  ensure (null permissionsLacking) (TokenLacksPermissions permissions)
  lift (cont tokenRow)
  where query = liftQuery $ getBy (UniqueToken token)

addReminder :: Token -> UTCTime -> Text -> DismissiveIO (Either TokenError ())
addReminder token sendAt text = authorized [PermissionCreate] token $ \tokenRow -> do
  let reminder = Reminder text sendAt False (tokenRowUserId tokenRow)
  insertReminder reminder

data AccountCreateError = EmailAlreadyExists
                        | TokenNonsense
                          deriving (Eq, Show)

createAccount :: EmailAddress -> DismissiveIO (Either AccountCreateError Token)
createAccount email = runEitherT $ do
  maybeDup <- (lift . liftQuery . insertBy) (User email)
  case maybeDup of
    Left  acct -> left EmailAlreadyExists
    Right userId -> liftEither (const TokenNonsense) =<< lift (createToken [PermissionCreate] userId)

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
  let PermSet { permReminder, permRead, permCreate, permEdit } = permSet
  let row = TokenRow token permRead permCreate permEdit userId permReminder
  lift (insertToken row)
  return token

insertToken :: TokenRow -> DismissiveIO (Key TokenRow)
insertToken = liftQuery . insert
