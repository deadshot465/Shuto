module Pick (pick) where
  
import Prelude

import Control.Promise (Promise, fromAff)
import Data.Array (drop, filter, length, mapWithIndex, reverse, snoc, sortBy, uncons, (!!))
import Data.Foldable (intercalate)
import Data.Int (fromString)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, null)
import Data.String (Pattern(..), contains, indexOf, split, splitAt, toLower, trim)
import Data.String as String
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Eris (DispatchableCommand, Message, createTextMessage)
import Utility (randomAff)

pick :: DispatchableCommand
pick = 
  { label: "pick"
  , generator: pickImpl
  , options:
    { aliases: []
    , deleteCommand: false
    , description: description
    , fullDescription: fullDescription
    , errorMessage: "エラー！"
    }
  }

pickImpl :: Message -> Array String -> Effect (Promise (Nullable Unit))
pickImpl msg args = fromAff $ run msg args
  where
    remainingOptions :: Array String -> String
    remainingOptions = intercalate ""

    run m@{ author: { mention } } args' = do
      let { head: times, tail } = fromMaybe { head: "", tail: [] } $ uncons args'
      let actualTimes = tryParseTimes times
      let options = remainingOptions tail
      result <- case actualTimes of
                  Nothing -> singlePick mention $ times <> " | " <> options
                  Just actualTimes' -> do
                    -- Try to extract options if there are options inside the times part.
                    -- E.g. 5000times|A|B|C
                    let trimmedOptions = extractOptions times options
                    if trimmedOptions == times then
                      singlePick mention trimmedOptions
                    else
                      multiplePick mention actualTimes' trimmedOptions
      _ <- createTextMessage m $ NonEmptyString result
      pure null

description :: String
description = "様々な選択肢に迷っている時、八谷に頼んで、八谷に選んでもらう。"

fullDescription :: String
fullDescription = "八谷に選択肢を選んでもらう。"

emptyMsg :: String
emptyMsg = "…選べる選択肢がないとどうすんの？"

tryParseTimes :: String -> Maybe Int
tryParseTimes times =
  if not $ contains timesPattern lowerTimes then Nothing
  else do
    let index = fromMaybe 0 $ indexOf timesPattern lowerTimes
    let { before } = splitAt index lowerTimes
    fromString before
  where
    timesPattern = Pattern "times"
    lowerTimes = toLower times

singlePick :: String -> String -> Aff String
singlePick authorName optionString = do
  let options = filterOptions optionString
  index <- randomAff 0 $ length options - 1
  let result = fromMaybe "" $ options !! index
  pure $ "⚾ " <> authorName <> "、俺が**" <> result <> "**を選んだ。"

multiplePick :: String -> Int -> String -> Aff String
multiplePick authorName times optionString =
  if String.null optionString then pure emptyMsg
  else do
    let options = filterOptions optionString
    let countMap = Map.fromFoldable $ (\s -> Tuple s 0) <$> options
    resultMap <- reverse <$> sortCountMap <$> runCount countMap options times
    pure $ "⚾ " <> authorName <> "、これは俺が頑張った結果だ。\n" <> (intercalate "\n" $ buildMultiplePickMsg resultMap)
  where
    runCount countMap _ 0 = pure countMap
    runCount countMap options times' = do
      index <- randomAff 0 $ length options - 1
      let choice = fromMaybe "" $ options !! index
      let currentCount = fromMaybe 0 $ Map.lookup choice countMap
      let newMap = Map.insert choice (currentCount + 1) countMap
      runCount newMap options (times' - 1)

extractOptions :: String -> String -> String
extractOptions timesPart existingOptions =
  if not $ contains pipe timesPart then
    if String.null existingOptions then timesPart else trim $ existingOptions
  else
    intercalate "|" $ snoc (drop 1 $ split pipe timesPart) existingOptions
  where pipe = Pattern "|"

filterOptions :: String -> Array String
filterOptions optionString = filter (not <<< String.null) $ trim <$> split (Pattern "|") optionString

sortCountMap :: Map String Int -> Array (Tuple String Int)
sortCountMap countMap = sortBy compareCount $ Map.toUnfoldable countMap

compareCount :: Tuple String Int -> Tuple String Int -> Ordering
compareCount (Tuple _ v1) (Tuple _ v2) = compare v1 v2

buildMultiplePickMsg :: Array (Tuple String Int) -> Array String
buildMultiplePickMsg resultArray = mapWithIndex buildMsg resultArray
  where
    buildMsg i (Tuple s v) = show (i + 1) <> "、" <> s <> "ー" <> show v <> "回"