module TwitterStream (startStream) where
  
import Prelude

import Constants (TokenType(..), bakugoColor, shutoColor)
import Data.Argonaut (class DecodeJson, decodeJson, jsonParser, printJsonDecodeError, (.:), (.:?))
import Data.Argonaut.Decode.Decoders (decodeJObject)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull, null)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (length)
import DiscordDispatcher (dispatchEmbed)
import Effect (Effect)
import Effect.Aff (Aff, Error, Fiber, Milliseconds(..), delay, error, forkAff, killFiber, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error) as Console
import Eris (CommandClient, Embed, _getTextChannel, createChannelEmbed, makeEmptyEmbed)
import Math (pow)
import Needle (twitterStreamConnect)
import Node.Buffer (Buffer, toString)
import Node.Encoding (Encoding(..))
import Node.Process (lookupEnv)
import Node.Stream (onData, onError)

newtype Data = Data { text :: String }
derive instance Generic Data _

instance DecodeJson Data where
  decodeJson json = do
    o <- decodeJObject json
    text <- o .: "text"
    pure $ Data { text }

instance Show Data where
  show = genericShow

newtype User = User
  { name :: String
  , profile_image_url :: String
  , username :: String
  }
derive instance Generic User _

instance DecodeJson User where
  decodeJson json = do
    o <- decodeJObject json
    name <- o .: "name"
    profile_image_url <- o .: "profile_image_url"
    username <- o .: "username"
    pure $ User { name, profile_image_url, username }

instance Show User where
  show = genericShow

newtype MediaItem = MediaItem
  { type :: String
  , url :: Maybe String
  }
derive instance Generic MediaItem _

instance DecodeJson MediaItem where
  decodeJson json = do
    o <- decodeJObject json
    t <- o .: "type"
    u <- o .:? "url"
    pure $ MediaItem { type: t, url: u }

instance Show MediaItem where
  show = genericShow

newtype Includes = Includes
  { media :: Maybe (Array MediaItem)
  , users :: Array User
  }
derive instance Generic Includes _

instance DecodeJson Includes where
  decodeJson json = do
    o <- decodeJObject json
    media <- o .:? "media"
    users <- o .: "users"
    pure $ Includes { media, users }

instance Show Includes where
  show = genericShow

newtype MatchedItem = MatchedItem
  { id :: String
  , tag :: String
  }

derive instance Generic MatchedItem _

instance DecodeJson MatchedItem where
  decodeJson json = do
    o <- decodeJObject json
    (id :: Number) <- o .: "id"
    tag <- o .: "tag"
    pure $ MatchedItem { id: show id, tag }

instance Show MatchedItem where
  show = genericShow

newtype StreamResponse = StreamResponse
  { data :: Data
  , includes :: Includes
  , matching_rules :: Array MatchedItem
  }
derive instance Generic StreamResponse _

instance DecodeJson StreamResponse where
  decodeJson json = do
    o <- decodeJObject json
    d <- o .: "data"
    includes <- o .: "includes"
    matching_rules <- o .: "matching_rules"
    pure $ StreamResponse { data: d, includes, matching_rules }

instance Show StreamResponse where
  show = genericShow

url :: String
url = "https://api.twitter.com/2/tweets/search/stream?media.fields=preview_image_url%2Curl&user.fields=name%2Cprofile_image_url%2Cusername&tweet.fields=created_at&expansions=attachments.media_keys%2Cauthor_id%2Creferenced_tweets.id"

timeout :: Int
timeout = 20000

animeMangaChannelId :: String
animeMangaChannelId = "763568486441418774"

userTwitterUrl :: String -> String
userTwitterUrl = append "https://twitter.com/"

defaultRetryAttempt :: Int
defaultRetryAttempt = 0

twitterLogo :: String
twitterLogo = "https://cdn.discordapp.com/attachments/811517007446671391/872399108466409472/apple-touch-icon-192x192.png"

baseEmbed :: Maybe User -> String -> Effect Embed
baseEmbed user text = do
  let author = case user of
        Nothing -> null
        Just (User { name, profile_image_url, username }) -> notNull $ { name, icon_url: notNull profile_image_url, url: notNull $ userTwitterUrl username }
  pure $ makeEmptyEmbed
    { author = author
    , color = notNull shutoColor
    , description = notNull text
    , footer = notNull $ { text: "Twitterを経由", icon_url: notNull twitterLogo }
    }

checkImage :: Maybe (Array MediaItem) -> Embed -> Embed
checkImage media embed = case media of
  Nothing -> embed
  Just e -> case e !! 0 of
    Nothing -> embed
    Just (MediaItem { type: t, url: u }) -> if t == "photo" then
                                              case u of
                                                Nothing -> embed
                                                Just u' -> embed { image = notNull $ { url: u' } } else embed

buildEmbed :: StreamResponse -> Effect Embed
buildEmbed (StreamResponse { data: Data { text }, includes: Includes { media, users } }) = do
  let user = users !! 0
  base <- baseEmbed user text
  pure $ checkImage media base

sendEmbed :: CommandClient -> Embed -> Aff Unit
sendEmbed client embed = do
  channel <- liftEffect $ _getTextChannel client animeMangaChannelId
  _ <- createChannelEmbed channel embed
  pure unit

getDispatcher :: StreamResponse -> CommandClient -> Embed -> Aff Unit
getDispatcher (StreamResponse { matching_rules: [ (MatchedItem { id }) ] }) client embed =
  case id of
    "1423324161796034561" -> dispatchEmbed Bakugo (embed { color = notNull bakugoColor })
    _ -> sendEmbed client embed
getDispatcher _ client embed = sendEmbed client embed

handleBufferData :: Buffer -> CommandClient -> Effect Unit
handleBufferData buffer client = do
  s <- toString UTF8 buffer
  if length s < 10 then pure unit
  else do
    case jsonParser s of
      Left err -> Console.error $ "Failed to parse string to Json: " <> err
      Right json -> case (decodeJson json :: _ StreamResponse) of
        Left err -> do
          Console.error s
          Console.error $ printJsonDecodeError err
        Right response -> do
          embed <- buildEmbed response
          launchAff_  $ getDispatcher response client embed

handleError :: Error -> Int -> Fiber Unit -> String -> CommandClient -> Effect Unit
handleError err retryAttempt dataFiber token client = launchAff_ do
  Console.error $ "An error occurred: " <> show err <> "\nRetrying...\n"
  killFiber (error "Streaming fiber terminated due to an error.") dataFiber
  delay $ Milliseconds $ pow 2.0 (toNumber retryAttempt)
  getStreamData token client (retryAttempt + 1)

getStreamData :: String -> CommandClient -> Int -> Aff Unit
getStreamData token client retryAttempt = do
  stream <- twitterStreamConnect token url timeout
  dataFiber <- forkAff $ liftEffect $ onData stream (\b -> handleBufferData b client)
  _ <- forkAff $ liftEffect $ onError stream (\e -> handleError e retryAttempt dataFiber token client)
  pure unit

startStream :: CommandClient -> Effect Unit
startStream client = do
  envResult <- lookupEnv "TWITTER_BEARER_TOKEN"
  case envResult of
    Nothing -> Console.error "TWITTER_BEARER_TOKEN is empty."
    Just token -> launchAff_ $ getStreamData token client defaultRetryAttempt