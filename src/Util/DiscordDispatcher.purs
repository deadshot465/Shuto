module DiscordDispatcher (dispatchEmbed) where

import Prelude

import Affjax (Request, defaultRequest)
import Affjax as Affjax
import Affjax.RequestBody (RequestBody)
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Constants (TokenType, getChannelId, getColor, getToken)
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Nullable (notNull, toMaybe)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Eris (EmbedAuthor, EmbedField, EmbedFooter, EmbedImage, EmbedThumbnail, Embed)

type SanitizedEmbedAuthor =
  { icon_url :: Maybe String
  , name :: String
  , url :: Maybe String
  }

type SanitizedEmbedField =
  { name :: String
  , value :: String
  , inline :: Maybe Boolean
  }

type SanitizedEmbedFooter =
  { text :: String
  , icon_url :: Maybe String
  }

type SanitizedEmbed =
  { author :: Maybe SanitizedEmbedAuthor
  , color :: Maybe Int
  , description :: Maybe String
  , fields :: Maybe (Array SanitizedEmbedField)
  , footer :: Maybe SanitizedEmbedFooter
  , image :: Maybe EmbedImage
  , thumbnail :: Maybe EmbedThumbnail
  , title :: Maybe String
  , url :: Maybe String
  , timestamp :: Maybe String
  }

apiEndpoint :: String
apiEndpoint = "https://discord.com/api/v9"

createMessageEndpoint :: String -> String
createMessageEndpoint channelId = apiEndpoint <> "/channels/" <> channelId <> "/messages"

sanitizeEmbedAuthor :: EmbedAuthor -> SanitizedEmbedAuthor
sanitizeEmbedAuthor { name, icon_url, url } =
  { name
  , icon_url: toMaybe icon_url
  , url: toMaybe url
  }

sanitizeEmbedField :: EmbedField -> SanitizedEmbedField
sanitizeEmbedField field@{ inline } = field { inline = toMaybe inline }

sanitizeEmbedFooter :: EmbedFooter -> SanitizedEmbedFooter
sanitizeEmbedFooter footer@{ icon_url } = footer { icon_url = toMaybe icon_url }

sanitizeEmbed :: Embed -> SanitizedEmbed
sanitizeEmbed embed =
  { author: sanitizeEmbedAuthor <$> toMaybe embed.author
  , color: toMaybe embed.color
  , description: toMaybe embed.description
  , fields: map sanitizeEmbedField <$> toMaybe embed.fields
  , footer: sanitizeEmbedFooter <$> toMaybe embed.footer
  , image: toMaybe embed.image
  , thumbnail: toMaybe embed.thumbnail
  , title: toMaybe embed.title
  , url: toMaybe embed.url
  , timestamp: toMaybe embed.timestamp
  }

buildRequestBody :: ∀ a. EncodeJson a => a -> RequestBody
buildRequestBody embed = RequestBody.Json $ encodeJson { embeds: [ embed ] }

buildRequest :: String -> Maybe RequestBody -> String -> Request String
buildRequest channelId content token = defaultRequest
  { url = createMessageEndpoint channelId
  , content = content
  , headers = [ (ContentType $ MediaType "application/json"), (RequestHeader "Authorization" ("Bot " <> token)), (RequestHeader "User-Agent" "DiscordBot ($url, $versionNumber)") ]
  , method = Left POST
  , responseFormat = ResponseFormat.string
  }

dispatchEmbed :: TokenType -> Embed -> Aff Unit
dispatchEmbed tokenType embed = do
  token <- liftEffect $ getToken tokenType (show tokenType <> "'s token is empty.")
  case token of
    Left err -> error err
    Right t -> do
      let channelId = getChannelId tokenType
      let color = getColor tokenType
      result <- bimap Affjax.printError identity <$> (Affjax.request $ buildRequest channelId (Just $ buildRequestBody $ sanitizeEmbed (embed { color = notNull color })) t)
      case result of
        Left e -> error e
        Right _ ->
          --log $ show s.status <> ": " <> s.statusText <> "\nbody: " <> s.body
          pure unit