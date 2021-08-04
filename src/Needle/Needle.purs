module Needle (Token, twitterStreamConnect, URL) where
  
import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Stream (Readable)

foreign import _twitterStreamConnect :: Token -> URL -> Int -> Effect (Readable ())

type Token = String
type URL = String

twitterStreamConnect :: Token -> URL -> Int -> Aff (Readable ())
twitterStreamConnect token url = liftEffect <<< _twitterStreamConnect token url