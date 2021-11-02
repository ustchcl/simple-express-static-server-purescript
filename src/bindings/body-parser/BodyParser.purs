module BodyParser (
  bodyParserJ,
  bodyParserU
) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn3)
import Node.Express.Handler (HandlerM(..), Handler)
import Node.Express.Types (Middleware)

foreign import bodyParserJson :: Middleware

bodyParserJ :: Handler
bodyParserJ = HandlerM \req res nxt -> liftEffect $ runEffectFn3 bodyParserJson req res nxt


foreign import bodyParserUrl :: Middleware
bodyParserU :: Handler
bodyParserU = HandlerM \req res nxt -> liftEffect $ runEffectFn3 bodyParserUrl req res nxt