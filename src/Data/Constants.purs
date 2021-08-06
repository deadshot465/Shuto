module Constants
  ( getChannelId
  , getColor
  , getStreamId
  , getToken
  , presences
  , pureScriptLogo
  , shutoPicture
  , TokenType(..)
  , updateDate
  , versionNumber
  )
  where

import Prelude

import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Node.Process (lookupEnv)

data TokenType = Shuto | Bakugo
derive instance Eq TokenType
derive instance Generic TokenType _

instance Show TokenType where
  show = genericShow

pureScriptLogo :: String  
pureScriptLogo = "https://cdn.discordapp.com/attachments/811517007446671391/871915061638230046/purescript-logo.png"

versionNumber :: String
versionNumber = "1.0.0"

updateDate :: String
updateDate = "2021-08-03"

shutoColor :: Int
shutoColor = 0x1D222D

bakugoColor :: Int
bakugoColor = 0xE3DAC9

getToken :: TokenType -> String -> Effect (Either String String)
getToken tokenType msg = note msg <$> (case tokenType of
  Shuto -> shutoToken
  Bakugo -> bakugoToken)

getChannelId :: TokenType -> String
getChannelId = case _ of
  Shuto -> animeMangaChannelId
  Bakugo -> bakugoChannelId

getColor :: TokenType -> Int
getColor = case _ of
  Shuto -> shutoColor
  Bakugo -> bakugoColor

getStreamId :: TokenType -> String
getStreamId = case _ of
  Shuto -> shutoStreamId
  Bakugo -> bakugoStreamId

shutoStreamId :: String
shutoStreamId = "1422767295227596805"

bakugoStreamId :: String
bakugoStreamId = "1423514056451788805"

bakugoToken :: Effect (Maybe String)
bakugoToken = lookupEnv "BAKUGO_TOKEN"

shutoToken :: Effect (Maybe String)
shutoToken = lookupEnv "TOKEN"

bakugoChannelId :: String
bakugoChannelId = "814879119389097995"
--bakugoChannelId = "828429184749404220"

animeMangaChannelId :: String
animeMangaChannelId = "763568486441418774"
--animeMangaChannelId = "828429184749404220"

presences :: Array String
presences = 
  [ "野球"
  , "寝る"
  , "雀さんとデート"
  , "雀さんとお祭り"
  , "雀さんと家に帰る"
  ]

shutoPicture :: String
shutoPicture = "https://cdn.discordapp.com/attachments/811517007446671391/871926499161280542/hachiya.png"