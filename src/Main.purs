module Main where

import Prelude

import Control.Promise (Promise, toAff)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Dotenv (loadFile) as Dotenv
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Eris (DispatchableCommand, _connectClient, _registerCommands, initializeClient)
import Ping (ping)

commands :: Array DispatchableCommand
commands = [ping]

main :: Effect Unit
main = launchAff_ do
  _ <- Dotenv.loadFile
  result <- liftEffect $ initializeClient
  case result of
    Left err -> error err
    Right c -> liftEffect $ do
      client <- c
      _ <- _registerCommands client commands
      _ <- _connectClient client
      pure unit

