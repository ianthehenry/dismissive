{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

import BasePrelude hiding (left)
import qualified Data.ByteString.Base16 as B16
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Logger
import Control.Monad.Trans.Either
import qualified Data.Configurator as Conf
import Dismissive.Api
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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

initialEmail :: EmailAddress -> Token -> LocalEmail
initialEmail recipient token = LocalEmail recipient "Dismissive" from from "Welcome to Dismissive" body
  where
    from = (address, Just "app")
    address = Text.intercalate "+" ["remind", (Text.decodeUtf8 . B16.encode) token]
    body = Text.intercalate "\n"
      [ "Welcome to Dismissive!"
      , ""
      , "Here's how it works: every time you send an email to this address:"
      , ""
      , address
      , ""
      , "We'll look at the subject to figure out a date (this is automatic; no human ever looks at your emails). If we can't figure out what the subject is supposed to mean (like \"Flurbsday at 14 AM\"), we'll fall back to \"seven days from now.\""
      , ""
      , "Once that date rolls around, we'll send the body of the message right back to you."
      , ""
      , "Cheers!"
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
dynamicServer = handleLandingPage :<|> handleAuth
  where
    handleAuth (Login "" email) = do
      createAccount email >>= \case
        Left EmailAlreadyExists -> send (loginEmail email)
        Left TokenNonsense -> (lift . lift) (left err500)
        Right token -> send (initialEmail email token)
      return $ layout (emailConfirm email)
    handleAuth _ = (return . layout . simplePage . div_) "Alright! Account successfully created."
    handleLandingPage = return (layout signupPage)
    send m = (lift . lift . handleSendError) =<< lift (sendMail m)

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
