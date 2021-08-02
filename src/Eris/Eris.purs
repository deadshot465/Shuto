module Eris
  ( connectClient
  , createTextMessage
  , editMessage
  , _registerCommands
  , CommandClient
  , CommandOptions
  , DispatchableCommand
  , initializeClient
  , Message) where

import Prelude

import Control.Promise (Promise, toAff)
import Data.Either (Either(..), note)
import Data.String.NonEmpty (NonEmptyString)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Process (lookupEnv)
  
foreign import _makeClient :: String -> ClientOptions -> String -> Effect CommandClient
foreign import _connectClient :: CommandClient -> Effect (Promise Unit)
foreign import _onMessageCreate :: CommandClient -> (Message -> Unit) -> Effect CommandClient
foreign import _createTextMessage :: Message -> NonEmptyString -> Effect (Promise Message)
foreign import _editMessage :: Message -> NonEmptyString -> Effect (Promise Message)
foreign import _registerCommands :: CommandClient -> Array DispatchableCommand -> Effect Unit

foreign import data CommandClient :: Type
foreign import data Message :: Type

type ClientOptions =
  { defaultImageFormat :: String
  , defaultImageSize :: Int
  }

type CommandOptions =
  { aliases :: Array String
  , deleteCommand :: Boolean
  , description :: String
  , fullDescription :: String
  }

type DispatchableCommand =
  { label :: String
  , generator :: Message -> Array String -> Effect Unit
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

makeClient :: String -> ClientOptions -> String -> Effect CommandClient
makeClient token clientOptions prefix = _makeClient token clientOptions prefix

initializeClient :: Effect (Either String (Effect CommandClient))
initializeClient = do
  token <- note "Token cannot be empty." <$> lookupEnv "TOKEN"
  prefix <- note "Prefix cannot be empty." <$> lookupEnv "PREFIX"
  let clientOptions = Right $ { defaultImageFormat: "png", defaultImageSize: 1024 }
  pure $ makeClient <$> token <*> clientOptions <*> prefix