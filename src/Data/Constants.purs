module Constants where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Node.Process (lookupEnv)

data TokenType = Bakugo

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

getToken :: TokenType -> Effect String
getToken tokenType = fromMaybe "" <$> (case tokenType of
  Bakugo -> bakugoToken)

getChannelId :: TokenType -> String
getChannelId = case _ of
  Bakugo -> bakugoChannelId

getColor :: TokenType -> Int
getColor = case _ of
  Bakugo -> bakugoColor

bakugoStreamId :: String
bakugoStreamId = "1423324161796034561"

bakugoToken :: Effect (Maybe String)
bakugoToken = lookupEnv "BAKUGO_TOKEN"

bakugoChannelId :: String
bakugoChannelId = "814879119389097995"

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