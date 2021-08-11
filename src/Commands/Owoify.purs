module Owoify (owoify) where
  
import Prelude

import Constants (getPrefix)
import Control.Promise (Promise, fromAff)
import Data.Nullable (Nullable, null)
import Data.Owoify.Owoify as Owoify
import Data.String (length, splitAt)
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Eris (DispatchableCommand, Message, createTextMessage)

owoify :: DispatchableCommand
owoify = 
  { label: "owoify"
  , generator: owoifyImpl
  , options:
    { aliases: []
    , deleteCommand: false
    , description: description
    , fullDescription: fullDescription
    , errorMessage: "エラー！"
    }
  }

owoifyImpl :: Message -> Array String -> Effect (Promise (Nullable Unit))
owoifyImpl msg _ = fromAff $ run msg
  where
    run m@{ content } = do
      prefix <- liftEffect $ getPrefix
      let splitIndex = length prefix + length "owoify "https://www.covid19-yamanaka.com/cont5/72.html
      let { after } = splitAt splitIndex content
      result <- liftEffect $ Owoify.owoify after Owoify.Uvu
      _ <- createTextMessage m $ NonEmptyString result
      pure null

description :: String
description = "八谷に頼んで、文字列を赤ちゃんが言いそうな言葉に転換する。"

fullDescription :: String
fullDescription = "八谷にレイテンシを問う。八谷に頼んで、文字列を赤ちゃんが言いそうな言葉に転換する。"