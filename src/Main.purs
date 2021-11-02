module Main where

import Prelude

import BodyParser (bodyParserJ, bodyParserU)
import Control.Comonad.Env (Env)
import Data.Array (index)
import Data.Int (fromString)
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (Ref, new, read)
import ErrorHandler (errorHandler)
import MethodOVerride (methodOverride)
import Node.Express.App (App, get, listenHttp, use)
import Node.Express.Middleware.Static (static)
import Node.Express.Response (send, sendFile)
import Node.Express.Types (Event, Port)
import Node.Globals (__dirname)
import Node.Path (concat)
import Node.Process (argv, lookupEnv)

import HtmlTemplate (test1)

type AppData = {
  version :: String
}
type AppState = Ref AppData


-- config
publicDir :: Effect String
publicDir = argv >>=
  \arr -> pure $ fromMaybe dir (index arr 2)
  where
    dir = __dirname <> "/public"

port :: Effect Int
port = do
  mb <- lookupEnv "PORT"
  pure $ fromMaybe 4567 $ mb >>= fromString


app :: Effect App 
app = publicDir >>= \d -> 
  pure $ get "/" $ sendFile (concat [d, "/index.html"])

initState :: Effect AppState
initState = new ({version: "1.0"})

url :: Effect String
url = port >>= \p -> pure $ "http://localhost:" <> show p

appSetup :: AppState -> App
appSetup state = do
  _ <- liftEffect $ read state
  liftEffect $ log "Setting up"
  dir <- liftEffect publicDir
  liftEffect $ log dir
  get "/" $ sendFile (concat [dir, "/index.html"])
  use methodOverride
  use bodyParserJ
  use bodyParserU
  use $ static dir
  use errorHandler

main :: Effect Unit
main = do 
  log test1



