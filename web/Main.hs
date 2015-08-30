{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

import BasePrelude hiding (left)
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Logger
import Control.Monad.Trans.Either
import qualified Data.Configurator as Conf
import Dismissive.Api
import Data.Text (Text)
import Dismissive.Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Lucid
import Landing
import Dismissive.Types
import Dismissive.Api

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

sendInitialEmail :: MonadIO m => EmailAddress -> Token -> m ()
sendInitialEmail email token = liftIO $ do
  print email
  print token
  print "done"

dynamicServer :: ServerT DynamicApi (DismissiveT (EitherT ServantErr IO))
dynamicServer = handleLandingPage :<|> handleAuth
  where
    handleAuth (Login "" email) = do
      createAccount email >>= \case
        Left EmailAlreadyExists -> return () -- maybe send an email here too, baby
        Left TokenNonsense -> lift (left err500)
        Right token -> sendInitialEmail email token -- send an email, baby
      return $ layout (emailConfirm email)
    handleAuth _ = (return . layout . simplePage . div_) "Alright! Account successfully created."
    handleLandingPage = return (layout signupPage)

server :: Dismiss -> Server Api
server nat = d :<|> serveDirectory "./static/"
  where d = enter nat dynamicServer

main :: IO ()
main = do
  conf <- Conf.load [Conf.Required "web.conf", Conf.Optional "shared.conf"]
  port <- Conf.require conf "port"
  connStr <- Conf.require conf "conn"

  putStrLn ("listening on port " <> show port)

  runStderrLoggingT $ getDismiss connStr $ \nat ->
    let application = serve api (server nat)
     in liftIO (run port application)
