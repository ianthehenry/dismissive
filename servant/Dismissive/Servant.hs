{-# LANGUAGE TypeOperators #-}

module Dismissive.Servant where

import BasePrelude
import Control.Monad.Trans.Either
import Control.Monad.Logger
import Dismissive.Api
import Servant

type Dismiss = DismissiveT (LoggingT (EitherT ServantErr IO)) :~> EitherT ServantErr IO

getDismiss :: ConnectionString -> Dismiss
getDismiss connStr = Nat (runStderrLoggingT . execDismissive connStr)
