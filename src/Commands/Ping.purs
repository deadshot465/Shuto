module Ping where
  
import Prelude

import Control.Promise (Promise, fromAff, toAff)
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.Time (diff)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds, forkAff, joinFiber, launchAff_)
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
    }
  }

pingImpl :: Message -> Array String -> Effect Unit
pingImpl msg _ = launchAff_ do
  past <- liftEffect nowTime
  sentMsg <- createTextMessage msg $ NonEmptyString "⚾️...待ってて"
  --sentFiber <- forkAff $ promise
  --sentMsg <- joinFiber sentFiber
  present <- liftEffect nowTime
  let difference = (diff present past :: Milliseconds)
  _ <- editMessage sentMsg $ NonEmptyString ("⚾️...ぽん。\nレイテンシは" <> show difference <> "だ。")
  pure unit

{- pingImpl :: ∀ a. Message -> Array a -> Aff Unit
pingImpl msg _ = liftEffect $ launchAff_ do
  sentFiber <- forkAff $ toAff $ _createTextMessage msg "⚾️...待ってて"
  past <- liftEffect nowTime
  sentMsg <- joinFiber sentFiber
  present <- liftEffect nowTime
  let difference = (diff present past :: Milliseconds)
  toAff $ _editMessage sentMsg $ "⚾️...ぽん。\nレイテンシは" <> show difference <> "だ。" -}

{- pingGenerator :: ∀ a. Message -> Array a -> Effect (Promise Unit)
pingGenerator msg = fromAff <<< pingImpl msg -}