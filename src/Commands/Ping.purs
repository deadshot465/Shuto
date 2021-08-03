module Ping (ping) where
  
import Prelude

import Control.Promise (Promise, fromAff)
import Data.Nullable (Nullable, null)
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.Time (diff)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Now (nowTime)
import Eris (DispatchableCommand, Message, createTextMessage, editMessage)

ping :: DispatchableCommand
ping = 
  { label: "ping"
  , generator: pingImpl
  , options:
    { aliases: []
    , deleteCommand: false
    , description: description
    , fullDescription: fullDescription
    , errorMessage: "エラー！"
    }
  }

pingImpl :: Message -> Array String -> Effect (Promise (Nullable Unit))
pingImpl msg _ = fromAff $ run msg
  where
    run m = do
      past <- liftEffect nowTime
      sentMsg <- createTextMessage m $ NonEmptyString "⚾...待ってて"
      present <- liftEffect nowTime
      let (Milliseconds milli) = (diff present past :: Milliseconds)
      _ <- editMessage sentMsg $ NonEmptyString ("⚾...ぽん。\nレイテンシは" <> show milli <> "ミリ秒だ。")
      pure null

description :: String
description = "八谷にレイテンシを問う。"

fullDescription :: String
fullDescription = "八谷にレイテンシを問う。"