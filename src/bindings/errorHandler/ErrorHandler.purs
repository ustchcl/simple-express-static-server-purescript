module ErrorHandler (
  errorHandler
) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn3)
import Node.Express.Handler (HandlerM(..), Handler)
import Node.Express.Types (Middleware)

foreign import errorHandler_ :: Middleware

errorHandler :: Handler
errorHandler = HandlerM \req res nxt -> liftEffect $ runEffectFn3 errorHandler_ req res nxt