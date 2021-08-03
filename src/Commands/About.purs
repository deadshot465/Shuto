module About (about) where
  
import Prelude

import Constants (pureScriptLogo, shutoColor, shutoPicture, updateDate, versionNumber)
import Control.Promise (Promise, fromAff)
import Data.Nullable (Nullable, notNull, null)
import Effect (Effect)
import Eris (DispatchableCommand, Embed, Message, createEmbed, makeEmptyEmbed)

about :: DispatchableCommand
about = 
  { label: "about"
  , generator: aboutImpl
  , options:
    { aliases: []
    , deleteCommand: false
    , description: description
    , fullDescription: fullDescription
    , errorMessage: "エラー！"
    }
  }

aboutImpl :: Message -> Array String -> Effect (Promise (Nullable Unit))
aboutImpl msg _ = fromAff $ run msg
  where
    run m = do
      _ <- createEmbed m aboutEmbed
      pure null

description :: String
description = "八谷の個人情報を取得する。"

fullDescription :: String
fullDescription = "八谷の個人情報を取得する、例えば使用されたプログラミング言語・フレームワーク、作者、バージョン番号など。"

aboutEmbed :: Embed
aboutEmbed = makeEmptyEmbed
  { author = notNull $ { name: "八谷鷲人", url: null, icon_url: notNull shutoPicture }
  , description = notNull $ "浅春観測の八谷鷲人。\n八谷鷲人はマンガ「浅春観測」の主人公の一人です。\n鷲人バージョン" <> versionNumber <> "の開発者：\n**Tetsuki Syu#1250**\n制作言語・フレームワーク：[PureScript](https://www.purescript.org/)と[Eris](https://abal.moe/Eris)ライブラリ。"
  , footer = notNull $ { text: "鷲人ボット：リリース" <> versionNumber <> " | " <> updateDate }
  , color = notNull shutoColor
  , thumbnail = notNull $ { url: pureScriptLogo }
  }