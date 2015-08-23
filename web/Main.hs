{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

import BasePrelude
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import qualified Data.Configurator as Conf
import Dismissive.Api
import Dismissive.Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type Api = Get '[JSON] ()

api :: Proxy Api
api = Proxy

server :: ServerT Api (DismissiveT (EitherT ServantErr IO))
server = return ()

server' :: DismissiveIO (Server Api)
server' = flip enter server <$> getDismiss

application :: DismissiveIO Application
application = makeApplication api server'

main :: IO ()
main = do
  conf <- Conf.load [Conf.Required "web.conf", Conf.Optional "shared.conf"]
  port <- Conf.require conf "port"
  connStr <- Conf.require conf "conn"

  putStrLn ("listening on port " <> show port)
  withDismissiveIO connStr $ do
    app <- application
    liftIO (run port app)
