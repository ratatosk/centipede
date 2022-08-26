{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Cmd 
    ( Cmd(..)
    , parseCmd
    , processCmd
    ) where

import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Except
import Data.List
import Data.Maybe
import Data.Tuple.Extra
import Data.Either.Extra
import qualified Data.HashMap.Strict as H
import Text.Parsec  
import Text.Parsec.Text

import Data.Text as T (Text, concat, pack)

import Types
import Util

data Cmd
    = Add Text Double UserHandle
    | Join [UserHandle]
    | Stat
    deriving (Show)

spaces1 :: Parser ()
spaces1 = skipMany1 space

double :: Parser Double
double = do
    int <- many1 digit
    frac <- option "" $ liftM2 (:) (char '.') (many digit)
    return $ read (int ++ frac)

userHandle :: Parser UserHandle
userHandle = pack <$> (char '@' >> many1 alphaNum)

addParser :: Parser Cmd
addParser = do
    string "add" >> spaces1
    item <- pack <$> (many1 $ satisfy (/= ':'))
    spaces >> char ':' >> spaces
    paid <- double
    spaces  
    user <- userHandle
    return $ Add item paid user

joinParser :: Parser Cmd
joinParser = do 
    string "join" >> spaces1
    users <- userHandle `sepBy1` spaces1
    return $ Join users

statParser :: Parser Cmd
statParser = string "stat" >> return Stat

cmdParser :: Parser Cmd
cmdParser = char '/' *> foldr1 (<|>) cmds <* eof

cmds :: [Parser Cmd]
cmds = 
    [ addParser
    , joinParser    
    , statParser
    ]
    
parseCmd :: Text -> Either String Cmd
parseCmd = mapLeft show . parse cmdParser ""

type ChatMM a = RWST Ctx [Text] (Maybe ChatState) (Except Text) a
type ChatM a = RWST Ctx [Text] ChatState (Except Text) a

withChatState :: ChatM a -> ChatMM a
withChatState a = RWST $ \r s -> case s of
    Nothing -> throwE "No active users, use /join"
    Just s' -> liftM (second3 Just) $ runRWST a r s'

processCmd :: Ctx -> Cmd -> BotM [Text]
processCmd ctx cmd = do
    let ci = (ctxChatId ctx)
    cs <- H.lookup ci <$> get
    case runExcept $ execRWST (process cmd) ctx cs of
        Right (cs', replies) -> do 
            modify $ H.alter (const cs') ci
            return replies
        Left err -> do  
            bark ["Error processing command: ", err]
            return [err]

process :: Cmd -> ChatMM ()
process (Add i s p) = withChatState $ processAdd i s p
process (Join us) = processJoin us
process (Stat) = rep ["Sorry, not implemented"]

defaultCs :: ChatState
defaultCs = ChatState
    { csUsers = []
    , csGroups = []
    , csItems = []
    }

rep :: MonadWriter [Text] m => [Text] -> m ()
rep = tell . singleton . T.concat

processJoin :: [UserHandle] -> ChatMM ()
processJoin users = do
    cs <- gets $ fromMaybe defaultCs
    let oldUsers = csUsers cs
    let present = intersect oldUsers users
    forM_ present $ \u -> rep ["Already added: @", u] 
    let newUsers = users \\ oldUsers
    forM_ newUsers $ \u -> rep ["Added: @", u] 
    let users' = oldUsers ++ newUsers
    put $ Just (cs {csUsers = users'})

processAdd :: Text -> Double -> UserHandle -> ChatM ()
processAdd i p u = do
    userId <- getUserId u
    allUsers <- allUserIds
    let item = Item { iName = i
                    , iPayers = [(userId, p)] 
                    , iUsers = zip allUsers (repeat 1)
                    }
    modify $ addItem item
    rep ["Item added"]

allUserIds :: ChatM [Int]
allUserIds = do
    nUsers <- gets $ length . csUsers
    return $ enumFromTo 0 (nUsers - 1)

addItem :: Item -> ChatState -> ChatState
addItem i cs = cs { csItems = newItems }
    where newItems = i : csItems cs

getUserId :: Text -> ChatM Int
getUserId u = do 
    userId <- gets $ elemIndex u . csUsers
    maybe (lift $ throwE $ T.concat ["User @", u, " isn't participating"]) return userId


    
