module Main where

import Prelude

import About (about)
import Constants (presences)
import Control.Monad.Rec.Class (forever)
import Data.Array (head, length, (!!))
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Hours(..), fromDuration)
import Dotenv (loadFile) as Dotenv
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Eris (CommandClient, DispatchableCommand, Message, _editStatus, _onMessageCreate, _onReady, _registerCommands, connectClient, initializeClient)
import Ping (ping)
import TwitterStream (startStream)
import Utility (randomAff)

commands :: Array DispatchableCommand
commands = [ping, about]

messageCreate :: Message -> Effect Unit
messageCreate _ = pure unit

updatePresence :: CommandClient -> Effect Unit
updatePresence client = launchAff_ $ forever do
  activity <- randomAff 0 (length presences) >>= \x -> pure $ presences !! x # fromMaybe (fromMaybe "" $ head presences)
  _ <- liftEffect $ _editStatus client activity
  delay $ fromDuration $ Hours 1.0

ready :: CommandClient -> Effect Unit
ready client = launchAff_ do
  liftEffect $ _onReady client $ updatePresence client

main :: Effect Unit
main = launchAff_ do
  _ <- Dotenv.loadFile
  result <- liftEffect $ initializeClient
  case result of
    Left err -> error err
    Right c -> do
      client <- liftEffect c
      _ <- liftEffect $ _registerCommands client commands
      _ <- liftEffect $ ready client
      _ <- liftEffect $ _onMessageCreate client messageCreate
      _ <- liftEffect $ startStream client
      _ <- connectClient client
      pure unit
