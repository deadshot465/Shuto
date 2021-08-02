module Eris
  ( _connectClient
  , _createTextMessage
  , _editMessage
  , _registerCommands
  , CommandClient
  , CommandOptions
  , DispatchableCommand
  , initializeClient
  , Message) where

import Prelude

import Control.Promise (Promise)
import Data.Either (Either(..), note)
import Effect (Effect)
import Node.Process (lookupEnv)
  
foreign import _makeClient :: String -> ClientOptions -> String -> Effect CommandClient
foreign import _connectClient :: CommandClient -> Effect (Promise Unit)
foreign import _onMessageCreate :: CommandClient -> (Message -> Unit) -> Effect CommandClient
foreign import _createTextMessage :: Message -> String -> Effect (Promise Message)
foreign import _editMessage :: Message -> String -> Effect (Promise Message)
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

makeClient :: String -> ClientOptions -> String -> Effect CommandClient
makeClient token clientOptions prefix = _makeClient token clientOptions prefix

initializeClient :: Effect (Either String (Effect CommandClient))
initializeClient = do
  token <- note "Token cannot be empty." <$> lookupEnv "TOKEN"
  prefix <- note "Prefix cannot be empty." <$> lookupEnv "PREFIX"
  let clientOptions = Right $ { defaultImageFormat: "png", defaultImageSize: 1024 }
  pure $ makeClient <$> token <*> clientOptions <*> prefix