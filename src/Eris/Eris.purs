module Eris
  ( _editStatus
  , _getTextChannel
  , _onMessageCreate
  , _onReady
  , _registerCommands
  , ActivityName
  , ChannelId
  , Command
  , CommandClient
  , CommandOptions
  , connectClient
  , createChannelEmbed
  , createChannelTextMessage
  , createEmbed
  , createTextMessage
  , DispatchableCommand
  , editMessage
  , Embed
  , EmbedAuthor
  , EmbedField
  , EmbedFooter
  , EmbedImage
  , EmbedThumbnail
  , initializeClient
  , makeEmptyEmbed 
  , Message
  , TextChannel
  , User) where

import Prelude

import Constants (TokenType(..), getToken)
import Control.Promise (Promise, toAff)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.Nullable (Nullable, null)
import Data.String.NonEmpty (NonEmptyString)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Process (lookupEnv)

foreign import _connectClient :: CommandClient -> Effect (Promise Unit)
foreign import _createChannelEmbed :: TextChannel -> Embed -> Effect (Promise Message)
foreign import _createChannelTextMessage :: TextChannel -> String -> Effect (Promise Message)
foreign import _createEmbed :: Message -> Embed -> Effect (Promise Message)
foreign import _createTextMessage :: Message -> NonEmptyString -> Effect (Promise Message)
foreign import _editMessage :: Message -> NonEmptyString -> Effect (Promise Message)
foreign import _editStatus :: CommandClient -> ActivityName -> Effect Unit
foreign import _getTextChannel :: CommandClient -> ChannelId -> Effect TextChannel
foreign import _makeClient :: Token -> ClientOptions -> CommandClientOptions -> Effect CommandClient
foreign import _onMessageCreate :: CommandClient -> (Message -> Effect Unit) -> Effect CommandClient
foreign import _onReady :: CommandClient -> Effect Unit -> Effect CommandClient
foreign import _registerCommands :: CommandClient -> Array DispatchableCommand -> Effect Unit

foreign import data CommandClient :: Type
foreign import data Command :: Type
foreign import data TextChannel :: Type

type Token = String
type ActivityName = String
type ChannelId = String

type User = 
  { bot :: Boolean
  }

type Message =
  { author :: User
  }

type Embed =
  { author :: Nullable EmbedAuthor
  , color :: Nullable Int
  , description :: Nullable String
  , fields :: Nullable (Array EmbedField)
  , footer :: Nullable EmbedFooter
  , image :: Nullable EmbedImage
  , thumbnail :: Nullable EmbedThumbnail
  , title :: Nullable String
  , url :: Nullable String
  , timestamp :: Nullable String
  }

type EmbedAuthor =
  { name :: String
  , url :: Nullable String
  , icon_url :: Nullable String
  }

type EmbedField =
  { name :: String
  , value :: String
  , inline :: Nullable Boolean
  }

type EmbedFooter = { text :: String, icon_url :: Nullable String }

type EmbedImage = { url :: String }

type EmbedThumbnail = { url :: String }

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

createChannelEmbed :: TextChannel -> Embed -> Aff Message
createChannelEmbed channel embed = do
  p <- liftEffect $ _createChannelEmbed channel embed
  toAff p

createChannelTextMessage :: TextChannel -> String -> Aff Message
createChannelTextMessage channel content = do
  p <- liftEffect $ _createChannelTextMessage channel content
  toAff p

createTextMessage :: Message -> NonEmptyString -> Aff Message
createTextMessage msg str = do
  p <- liftEffect $ _createTextMessage msg str
  toAff p

createEmbed :: Message -> Embed -> Aff Message
createEmbed msg embed = do
  p <- liftEffect $ _createEmbed msg embed
  toAff p

defaultPrefix :: String
defaultPrefix = "sh?"

editMessage :: Message -> NonEmptyString -> Aff Message
editMessage msg str = do
  p <- liftEffect $ _editMessage msg str
  toAff p

makeClient :: Token -> ClientOptions -> CommandClientOptions -> Effect CommandClient
makeClient token clientOptions commandClientOptions = _makeClient token clientOptions commandClientOptions

makeEmptyEmbed :: Embed
makeEmptyEmbed =
  { author: null
  , color: null
  , description: null
  , fields: null
  , footer: null
  , image: null
  , thumbnail: null
  , title: null
  , url: null
  , timestamp: null
  }

initializeClient :: Effect (Either String (Effect CommandClient))
initializeClient = do
  token <- getToken Shuto "Token cannot be empty."
  prefix <- fromMaybe defaultPrefix <$> lookupEnv "PREFIX"
  let clientOptions = Right $ { defaultImageFormat: "png", defaultImageSize: 1024 }
  let commandClientOptions = Right $ { ignoreBots: true
                                     , ignoreSelf: true
                                     , description: "八谷鷲人"
                                     , name: "八谷鷲人"
                                     , prefix: prefix
                                     }
  pure $ makeClient <$> token <*> clientOptions <*> commandClientOptions