module Main where

import Prelude

import Control.Comonad.Env (Env)
import Data.Array (index)
import Data.Int (fromString)
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Node.Express.App (App, get, listenHttp)
import Node.Express.Response (send, sendFile)
import Node.Express.Types (Event, Port)
import Node.Globals (__dirname)
import Node.Path (concat)
import Node.Process (argv, lookupEnv)


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



url :: Effect String
url = port >>= \p -> pure $ "http://localhost:" <> show p

main :: Effect Unit
main = do
  a <- app
  p <- port
  u <- url
  dir <- publicDir
  void $ listenHttp a p \_ -> log $ "Listening " <> u <> dir




