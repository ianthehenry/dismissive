{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

import BasePrelude
import Control.Monad.IO.Class
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

dynamicServer :: ServerT DynamicApi (DismissiveT (EitherT ServantErr IO))
dynamicServer = handleLandingPage :<|> handleAuth
  where
    handleAuth (Login username email) = return (layout $ simplePage $ div_ $ toHtml $ "okay!" <> email)
    handleLandingPage = return (layout signupPage)

server :: DismissiveIO (Server Api)
server = do
  dismiss <- getDismiss
  let d = enter dismiss dynamicServer
  return (d :<|> serveDirectory "./static/")

application :: DismissiveIO Application
application = makeApplication api server

main :: IO ()
main = do
  conf <- Conf.load [Conf.Required "web.conf", Conf.Optional "shared.conf"]
  port <- Conf.require conf "port"
  connStr <- Conf.require conf "conn"

  putStrLn ("listening on port " <> show port)
  withDismissiveIO connStr $ do
    app <- application
    liftIO (run port app)
