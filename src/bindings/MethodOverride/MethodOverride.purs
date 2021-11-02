module MethodOVerride (
  methodOverride
) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn3)
import Node.Express.Handler (HandlerM(..), Handler)
import Node.Express.Types (Middleware)

foreign import methodOverrideNoParam :: Middleware

methodOverride :: Handler
methodOverride = HandlerM \req res nxt -> liftEffect $ runEffectFn3 methodOverrideNoParam req res nxt