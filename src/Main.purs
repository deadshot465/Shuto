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
import Node.Process (lookupEnv)

foreign import _makeClient :: String -> ClientOptions -> String -> CommandClient
foreign import _connectClient :: CommandClient -> Promise Unit
foreign import _onMessageCreate :: CommandClient -> (Message -> Unit) -> CommandClient

foreign import data CommandClient :: Type
foreign import data Message :: Type

type ClientOptions =
  { defaultImageFormat :: String
  , defaultImageSize :: Int
  }

makeClient :: Either String String -> ClientOptions -> Either String String -> Either String CommandClient
makeClient token clientOptions prefix = case token of
  Right t -> case prefix of
    Right p -> Right $ _makeClient t clientOptions p
    Left err -> Left err
  Left err -> Left err

initializeClient :: Effect (Maybe CommandClient)
initializeClient = do
  token <- note "Token cannot be empty." <$> lookupEnv "TOKEN"
  prefix <- note "Prefix cannot be empty." <$> lookupEnv "PREFIX"
  let clientOptions = { defaultImageFormat: "png", defaultImageSize: 1024 }
  case makeClient token clientOptions prefix of
    Left err -> do
      _ <- error err
      pure Nothing
    Right c -> pure $ Just c

main :: Effect Unit
main = launchAff_ do
  _ <- Dotenv.loadFile
  result <- liftEffect $ initializeClient
  case result of
    Nothing -> error "Client initialization failed."
    Just c -> toAff $ _connectClient c

