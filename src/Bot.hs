{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Bot where

import Data.Maybe
import Data.Either.Extra

import Control.Monad.Except
import Control.Monad.Loops
import Control.Monad.IO.Class()

import Control.Concurrent

import Network.HTTP.Client (responseTimeoutMicro, responseTimeout)

import Servant.Client (ClientEnv, makeClientRequest, defaultMakeClientRequest)

import Telegram.Bot.API.Types
import Telegram.Bot.API.Methods
import Telegram.Bot.API.GettingUpdates
import Telegram.Bot.API.MakingRequests

import qualified Data.Text as T

import Cmd
import Types
import Util

updateTimeoutSec :: Int
updateTimeoutSec = 600

messageCtx :: Message -> Either String Ctx
messageCtx msg = do
    let chat = chatId $ messageChat msg
    user <- maybeToEither "No user" (messageFrom msg)
    userName <- maybeToEither "No username" (userUsername user)
    return $ Ctx userName chat

messageCmd :: Message -> Either String Cmd
messageCmd msg = maybeToEither "No text" (messageText msg) >>= parseCmd

reply :: Ctx -> T.Text -> BotM ()
reply ctx txt = sendRequest (lift $ sendMessage req) >> return () 
    where
        req = SendMessageRequest (SomeChatId (ctxChatId ctx)) txt
            Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    
processUpdate :: Update -> BotM ()
processUpdate upd = 
    case updateMessage upd of
        Just msg -> processMsg msg
        Nothing -> bark ["No message"]

processMsg :: Message -> BotM ()
processMsg msg = 
    case messageCtx msg of
        Left err -> liftIO $ putStrLn err
        Right ctx ->
            case messageCmd msg of
                Left err -> do
                    liftIO $ putStrLn err
                    reply ctx (T.pack err)
                Right cmd -> processCmd ctx cmd >>= mapM_ (reply ctx)

sendRequest :: BotM (Response a) -> BotM (Maybe a)
sendRequest act = do
    result <- (Right <$> act) `catchError` (return . Left . T.pack . show)
    case result of
        Left err -> do
            bark ["Transport error: ", err]
            liftIO $ threadDelay 5000000
            return Nothing
        Right resp -> if responseOk resp
            then return $ Just (responseResult resp)
            else do
                bark ["Server error: ", (fromMaybe "unknown" (responseDescription resp))]
                liftIO $ threadDelay 5000000
                return Nothing

fetchUpdateBatch :: Maybe UpdateId -> BotM [Update]
fetchUpdateBatch ofs = liftM concat $ sendRequest (lift $ getUpdates req)
    where
        req = GetUpdatesRequest { getUpdatesOffset = ofs
                                , getUpdatesLimit = Nothing
                                , getUpdatesTimeout = Just (Seconds updateTimeoutSec)
                                , getUpdatesAllowedUpdates = Nothing }

fetchUpdates :: ClientEnv -> IO ()
fetchUpdates env = runBotM (iterateM_ go Nothing) env >> return ()
    where 
        inc (UpdateId i) = UpdateId (i+1)
        go ofs = do
            updates <- fetchUpdateBatch ofs
            mapM_ processUpdate updates
            let newOfs = if null updates
                then ofs 
                else Just (inc . maximum $ map updateUpdateId updates)
            return newOfs 

run :: Token -> IO ()
run token = do
    let rto = responseTimeoutMicro $ 1000000 * updateTimeoutSec
        mkReq b r = (defaultMakeClientRequest b r) {responseTimeout = rto}
    env <- defaultTelegramClientEnv token
    fetchUpdates env {makeClientRequest = mkReq}

botMain :: IO ()
botMain = do
  putStrLn "Please, enter Telegram bot's API token:"
  token <- Token . T.pack <$> getLine
  run token