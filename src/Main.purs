module Main where

import Prelude

import Data.Either (Either(..))
import Dotenv (loadFile) as Dotenv
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Eris (DispatchableCommand, Message, _onMessageCreate, _registerCommands, connectClient, initializeClient)
import Ping (ping)

commands :: Array DispatchableCommand
commands = [ping]

messageCreate :: Message -> Effect Unit
messageCreate msg = pure unit

main :: Effect Unit
main = launchAff_ do
  _ <- Dotenv.loadFile
  result <- liftEffect $ initializeClient
  case result of
    Left err -> error err
    Right c -> do
      client <- liftEffect c
      _ <- liftEffect $ _registerCommands client commands
      _ <- liftEffect $ _onMessageCreate client messageCreate
      _ <- connectClient client
      pure unit
