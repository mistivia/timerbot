{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.Char
import Data.Text (Text)
import Data.List (intercalate)
import Data.Time
import Data.Time.Clock.POSIX

import qualified Data.Text as Text

import Text.Read (readMaybe)

import Control.Monad.Writer

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser (updateMessageText)

data Event = Event { eventTime :: Integer, eventChatId :: ChatId, eventMsgId :: MessageId }

data Action
  = TextMsg { textMsgText :: Text, textMsgChatId :: ChatId , textMsgId :: MessageId }
  | AddEvent Event
  | FireEvents Integer

type Model = [Event]

main :: IO ()
main = do
  content <- readFile "tgtoken"
  let token = Token $ Text.pack $ strip content
  run token

currentTimestamp :: IO Integer
currentTimestamp = fmap (floor . utcTimeToPOSIXSeconds) getCurrentTime

botApp :: BotApp Model Action
botApp = BotApp
  { botInitialModel = []
  , botAction = updateToAction
  , botHandler = handleAction
  , botJobs = [
      BotJob
        { botJobSchedule = "* * * * *"  -- every minute
        , botJobTask = eventHandler
        }]
  }

eventHandler :: Model -> Eff Action Model
eventHandler model = Eff $ do
  let acts = do
       ts <- liftIO $ currentTimestamp
       return $ Just $ FireEvents ts
  tell [acts]
  return model

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _ = do
    text <- updateMessageText update
    cid <- fmap chatId $ fmap messageChat $ updateMessage update
    mid <- fmap messageMessageId $ updateMessage update
    return $ TextMsg text cid mid

replyTextEff :: Model -> ChatId -> MessageId -> Text -> Eff Action Model
replyTextEff model cid mid msg = Eff $ do  
    let acts = do
         replyToMessage cid mid msg
         return Nothing
    tell [acts]
    return model

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  TextMsg tmsg cid mid -> case (words $ Text.unpack tmsg) of
    (command:args) ->
      case command of
        _ | command == "/timer" -> handleTimerCmd model cid mid args
          | otherwise -> replyTextEff model cid mid "无效命令"
    [] -> replyTextEff model cid mid "无效命令"
  AddEvent e -> return (e:model)
  FireEvents ts -> fireEvents model ts

replyToMessage:: ChatId -> MessageId -> Text -> BotM ()
replyToMessage cid mid t = replyTo (SomeChatId cid) $ ReplyMessage
    t Nothing Nothing Nothing Nothing Nothing Nothing (Just mid) Nothing Nothing 

fireEvents :: Model -> Integer -> Eff Action Model
fireEvents model ts = Eff $ do
    let acts = do
         let sendEvent e = do
              replyToMessage (eventChatId e) (eventMsgId e) "时间到了哦"
         mapM_ sendEvent $ filter (\e -> (eventTime e) <= ts) model
         return Nothing
    tell [acts]
    return $ filter (\e -> (eventTime e) > ts) model

handleTimerCmd :: Model -> ChatId -> MessageId -> [String] -> Eff Action Model
handleTimerCmd model chatId mid args =
  let minutes :: Maybe Int = readMaybe $ intercalate " " args in
    case minutes of
      Nothing -> replyTextEff model chatId mid "格式：/timer <分钟数>" 
      Just num ->
        Eff $ do
          let acts = do
                replyToMessage chatId mid $ Text.pack $ "已设置" ++ (show num) ++ "分钟提醒"
                curTime <- liftIO $ currentTimestamp
                return $ Just $ AddEvent $ Event (curTime + (toInteger num) * 60) chatId mid
          tell [acts]
          return model

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ botApp env

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace


