module Eris
  ( Command
  , connectClient
  , createTextMessage
  , editMessage
  , _onMessageCreate
  , _registerCommands
  , CommandClient
  , CommandOptions
  , DispatchableCommand
  , initializeClient
  , Message
  , User) where

import Prelude

import Control.Promise (Promise, toAff)
import Data.Either (Either(..), note)
import Data.Maybe (fromMaybe)
import Data.Nullable (Nullable)
import Data.String.NonEmpty (NonEmptyString)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Process (lookupEnv)
  
foreign import _makeClient :: String -> ClientOptions -> CommandClientOptions -> Effect CommandClient
foreign import _connectClient :: CommandClient -> Effect (Promise Unit)
foreign import _onMessageCreate :: CommandClient -> (Message -> Effect Unit) -> Effect CommandClient
foreign import _createTextMessage :: Message -> NonEmptyString -> Effect (Promise Message)
foreign import _editMessage :: Message -> NonEmptyString -> Effect (Promise Message)
foreign import _registerCommands :: CommandClient -> Array DispatchableCommand -> Effect Unit

foreign import data CommandClient :: Type
foreign import data Command :: Type

type User = 
  { bot :: Boolean
  }

type Message =
  { author :: User
  }

type ClientOptions =
  { defaultImageFormat :: String
  , defaultImageSize :: Int
  }

type CommandOptions =
  { aliases :: Array String
  , deleteCommand :: Boolean
  , description :: String
  , fullDescription :: String
  , errorMessage :: String
  }

type CommandClientOptions =
  { ignoreBots :: Boolean
  , ignoreSelf :: Boolean
  , description :: String
  , name :: String
  , prefix :: String
  }

type DispatchableCommand =
  { label :: String
  , generator :: Message -> Array String -> Effect (Promise (Nullable Unit))
  , options :: CommandOptions
  }

connectClient :: CommandClient -> Aff Unit
connectClient client = do
  p <- liftEffect $ _connectClient client
  toAff p

createTextMessage :: Message -> NonEmptyString -> Aff Message
createTextMessage msg str = do
  p <- liftEffect $ _createTextMessage msg str
  toAff p

editMessage :: Message -> NonEmptyString -> Aff Message
editMessage msg str = do
  p <- liftEffect $ _editMessage msg str
  toAff p

makeClient :: String -> ClientOptions -> CommandClientOptions -> Effect CommandClient
makeClient token clientOptions commandClientOptions = _makeClient token clientOptions commandClientOptions

defaultPrefix :: String
defaultPrefix = "sh?"

initializeClient :: Effect (Either String (Effect CommandClient))
initializeClient = do
  token <- note "Token cannot be empty." <$> lookupEnv "TOKEN"
  prefix <- fromMaybe defaultPrefix <$> lookupEnv "PREFIX"
  let clientOptions = Right $ { defaultImageFormat: "png", defaultImageSize: 1024 }
  let commandClientOptions = Right $ { ignoreBots: true
                                     , ignoreSelf: true
                                     , description: "八谷鷲人"
                                     , name: "八谷鷲人"
                                     , prefix: prefix
                                     }
  pure $ makeClient <$> token <*> clientOptions <*> commandClientOptions