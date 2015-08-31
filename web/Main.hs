{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

import BasePrelude hiding (left)
import qualified Data.ByteString.Base16 as B16
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Either
import qualified Data.Configurator as Conf
import Dismissive.Api
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock
import Dismissive.Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Lucid
import Landing
import Dismissive.Types
import Dismissive.Api
import Dismissive.Mailer

data Login = Login Text Text

instance FromFormUrlEncoded Login where
  fromFormUrlEncoded [("username", u), ("email", e)] = Right (Login u e)
  fromFormUrlEncoded _ = Left "that doesn't look right"

type DynamicApi = Get '[HTML] (Html ())
             :<|> "auth" :> ReqBody '[FormUrlEncoded] Login :> Post '[HTML] (Html ())
             :<|> WaitingApi
type WaitingApi = "waiting" :> Capture "email" EmailAddress :> Get '[HTML] (Html ())
type StaticApi = "static" :> Raw
type Api = DynamicApi :<|> StaticApi

api :: Proxy Api
api = Proxy

data Signup = Signup

layout :: Monad m => Partial m -> HtmlT m ()
layout (stylesheets, scripts, inner) = html_ (head_ head <> body_ inner)
  where
    head = do
      title_ "Dismissive"
      traverse_ css_ ("/static/main.css":stylesheets)
      traverse_ js_ scripts
    css_ path = link_ [type_ "text/css", rel_ "stylesheet", href_ path]
    js_ :: Monad m => Text -> HtmlT m ()
    js_ path = script_ [type_ "text/javascript", src_ path] ("" :: Text)

simplePage :: HtmlT m () -> Partial m
simplePage = ([], [], )

emailConfirm :: Monad m => EmailAddress -> Partial m
emailConfirm email = simplePage $ do
  h1_ "Welcome to Dismissive"
  (p_ . toHtml) ("An email is on its way to " <> email <> " with all the details.")
  a_ [href_ "https://en.wikipedia.org/wiki/Special:Random"] "Check out a random Wikipedia article while you wait."

initialEmail :: EmailAddress -> Token -> Text -> LocalEmail
initialEmail recipient token domain = LocalEmail recipient "Dismissive" from from "Welcome to Dismissive" body
  where
    from = (local, Just "app")
    local = Text.intercalate "-" ["remind", (Text.decodeUtf8 . B16.encode) token]
    body = Text.intercalate "\n"
      [ "Welcome to Dismissive!"
      , ""
      , "Here's how it works: every time you send an email to this address:"
      , ""
      , local <> "@app." <> domain
      , ""
      , "We'll look at the subject to figure out a date (this is automatic; no human ever looks at your emails). If we can't figure out what the subject is supposed to mean (like \"Flurbsday at 14 AM\"), we'll fall back to \"seven days from now.\""
      , ""
      , "Once that date rolls around, we'll send the body of the message right back to you."
      , ""
      , "To get you started off, I've scheduled a reminder for five minutes from now and another for one week out."
      , ""
      , "Happy remindering!"
      ]

initialReminder :: Text
initialReminder = Text.intercalate "\n"
  [ "You can reply to reminder emails to snooze them until later. Just include the new date you want to be reminded as the first line of the body, or leave it blank for a 24 hour snooze."
  , ""
  , "Try it out! Reply to this email now with the body \"five minutes from now\" (or whenever you'd like)."
  ]

checkInReminder :: Text
checkInReminder = Text.intercalate "\n"
  [ "Hey there!"
  , ""
  , "You signed up for Dismissive one week ago. How's it been so far?"
  , ""
  , "You can always email thoughts@dismissive.io with any feedback, problems, or suggestions you have."
  , ""
  , "Stay classy!"
  ]

loginEmail :: EmailAddress -> LocalEmail
loginEmail recipient = LocalEmail recipient "Dismissive" ("hello", Nothing) ("help", Nothing) "Hello again!" body
  where
    body = Text.intercalate "\n"
      [ "Hey! You already have a Dismissive account. Did you lose your token?"
      , ""
      , "There's currently no way to generate a new one. I just haven't written that feature yet."
      , ""
      , "Instead, reply to this message and I'll try to help you out."
      ]

handleSendError :: Either SendError () -> EitherT ServantErr IO ()
handleSendError (Left _) = left err400
handleSendError _ = (lift . return) ()

dynamicServer :: ServerT DynamicApi (DismissiveT (MailerT (EitherT ServantErr IO)))
dynamicServer = handleLandingPage :<|> handleAuth :<|> handleWaiting
  where
    handleAuth (Login "" email) = do
      createAccount email >>= \case
        Left EmailAlreadyExists -> send (loginEmail email)
        Left TokenNonsense -> liieft err500
        Right token -> do
          domain <- lift (asks mailerDomain)
          send (initialEmail email token domain)
          now <- liftIO getCurrentTime
          addReminder token (after (Minutes 5) now) initialReminder
          addReminder token (after (Weeks 1) now) checkInReminder
          return ()
      redirect' (safeLink api (Proxy :: Proxy WaitingApi) email)
    handleAuth _ = (return . layout . simplePage . div_) "Alright! Account successfully created."
    handleLandingPage = return (layout signupPage)
    send m = (lift . lift . handleSendError) =<< lift (sendMail m)
    handleWaiting email = return $ layout (emailConfirm email)
    liieft = lift . lift . left
    redirect' = liieft . redirect

redirect :: URI -> ServantErr
redirect uri = err303 { errHeaders = [("Location", loc)] }
  where loc = (Text.encodeUtf8 . Text.pack . show) uri

data Duration = Weeks Integer | Minutes Integer

after :: Duration -> UTCTime -> UTCTime
after delta = addUTCTime (realToFrac (fromDuration delta))

fromDuration :: Duration -> NominalDiffTime
fromDuration (Weeks n) = fromDuration $ Minutes (n * 7 * 24 * 60)
fromDuration (Minutes n) = (realToFrac . secondsToDiffTime) (n * 60)

server :: Dismiss -> Server Api
server nat = d :<|> serveDirectory "./static/"
  where d = enter nat dynamicServer

main :: IO ()
main = do
  conf <- Conf.load [Conf.Required "web.conf", Conf.Optional "shared.conf"]
  port <- Conf.require conf "port"
  connStr <- Conf.require conf "conn"
  mailer <- MailerConf <$> Conf.require conf "mandrill-key"
                       <*> Conf.require conf "mail-domain"

  putStrLn ("listening on port " <> show port)

  runStderrLoggingT $ getDismiss connStr mailer $ \nat ->
    let application = serve api (server nat)
     in liftIO (run port application)
