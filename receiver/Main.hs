{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

import BasePrelude
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Logger
import Control.Monad.Trans.Either
import qualified Data.Configurator as Conf
import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.ByteString.Base16 as B16
import Dismissive.Api
import Dismissive.Servant
import Dismissive.Types
import Dismissive.Mailer
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Types

type Api = ReqBody '[FormUrlEncoded] [MandrillEvent] :> Post '[JSON] ()

api :: Proxy Api
api = Proxy

data Action = ActionSnooze | ActionCreate deriving Eq

parseAction :: Text -> Maybe Action
parseAction "remind" = Just ActionCreate
parseAction "snooze" = Just ActionSnooze
parseAction _ = Nothing

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

firstTwo :: [a] -> Maybe (a, a)
firstTwo (x:y:_) = Just (x, y)
firstTwo _ = Nothing

parseToken :: Text -> Maybe Token
parseToken = ensureFull . B16.decode . Text.encodeUtf8
  where
    ensureFull (token, "") | BS.length token > 8 = Just token
    ensureFull _ = Nothing

bimapA :: Applicative f => (a -> f b) -> (c -> f d) -> (a, c) -> f (b, d)
bimapA f g (a, b) = (,) <$> f a <*> g b

parseTo :: Message -> Maybe (Action, Token)
parseTo message = do
  local <- (safeHead . Text.split (== '@') . messageTo) message
  x <- firstTwo (Text.split (== '-') local)
  bimapA parseAction parseToken x

handleMessage :: Text -> Message -> DismissiveIO ()
handleMessage "inbound" message@(parseTo -> Just (ActionCreate, token)) = do
  now <- liftIO getCurrentTime
  void $ addReminder token now (messageBody message)
handleMessage _ _ = return ()

uncurryEvent :: (Text -> Message -> a) -> MandrillEvent -> a
uncurryEvent f MandrillEvent { event, msg } = f event msg

server :: ServerT Api (DismissiveT (MailerT (EitherT ServantErr IO)))
server events = for_ events (uncurryEvent handleMessage)

server' :: Dismiss -> Server Api
server' nat = enter nat server

main :: IO ()
main = do
  conf <- Conf.load [Conf.Required "receiver.conf", Conf.Optional "shared.conf"]
  port <- Conf.require conf "port"
  connStr <- Conf.require conf "conn"
  mailer <- MailerConf <$> Conf.require conf "mandrill-key"
                       <*> Conf.require conf "mail-domain"

  putStrLn ("listening on port " <> show port)

  runStderrLoggingT $ getDismiss connStr mailer $ \nat ->
    let application = serve api (server' nat)
     in liftIO (run port application)
