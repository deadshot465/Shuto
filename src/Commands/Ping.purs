module Ping where
  
import Prelude

import Control.Promise (Promise, fromAff)
import Data.Nullable (Nullable, null)
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.Time (diff)
import Effect (Effect)
import Effect.Aff (Milliseconds)
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
    , description: "八谷にレイテンシを問う。"
    , fullDescription: "八谷にレイテンシを問う。"
    , errorMessage: "エラー！"
    }
  }

pingImpl :: Message -> Array String -> Effect (Promise (Nullable Unit))
pingImpl msg _ = fromAff $ run msg
  where
    run m = do
      past <- liftEffect nowTime
      sentMsg <- createTextMessage m $ NonEmptyString "⚾️...待ってて"
      present <- liftEffect nowTime
      let difference = (diff present past :: Milliseconds)
      _ <- editMessage sentMsg $ NonEmptyString ("⚾️...ぽん。\nレイテンシは" <> show difference <> "だ。")
      pure null