{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Cmd 
    ( Cmd(..)
    , parseCmd
    , processCmd
    ) where

import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.Except
import Data.List
import Data.Maybe
import Data.Array (elems, accumArray)
import Data.Tuple.Extra
import Data.Either.Extra
import qualified Data.HashMap.Strict as H
import Text.Parsec  
import Text.Parsec.Text
import Data.Text as T (Text, pack, unlines)
import Data.String.Interpolate (i)

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
    Nothing -> throwError "No active users, use /join"
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
process (Add n s p) = withChatState $ processAdd n s p
process (Join us) = processJoin us
process (Stat) = withChatState processStats

defaultCs :: ChatState
defaultCs = ChatState
    { csUsers = []
    , csGroups = []
    , csItems = []
    }

rep :: MonadWriter [Text] m => Text -> m ()
rep = tell . singleton

processJoin :: [UserHandle] -> ChatMM ()
processJoin users = do
    cs <- gets $ fromMaybe defaultCs
    let oldUsers = csUsers cs
    let present = intersect oldUsers users
    forM_ present $ \u -> rep [i|"Already added: @#{u}"|]
    let newUsers = users \\ oldUsers
    forM_ newUsers $ \u -> rep [i|"Added: @#{u}"|]
    let users' = oldUsers ++ newUsers
    put $ Just (cs {csUsers = users'})

processAdd :: Text -> Double -> UserHandle -> ChatM ()
processAdd n p u = do
    userId <- getUserId u
    allUsers <- allUserIds
    let item = Item { iName = n
                    , iPayers = [(userId, p)] 
                    , iUsers = zip allUsers (repeat 1)
                    }
    modify $ addItem item
    rep "Item added"

processStats :: ChatM ()
processStats = do
    items <- gets csItems
    users <- gets csUsers
    let n = length users
    let payed = calcPayers n $ map iPayers items
    let prices = calcPrices $ map iPayers items
    let used = calcUsers n $ zip (map iUsers items) prices
    rep $ formatStats $ zip3 users payed used

calcPayers :: Int -> [[(Int, Double)]] -> [Double]
calcPayers n p = elems $ accumArray (+) 0 (0, n-1) (concat p)

calcPrices :: [[(Int, Double)]] -> [Double]
calcPrices = map (sum . map snd)

calcUsers :: Int -> [([(Int, Double)], Double)] -> [Double]
calcUsers n items = elems $ accumArray (+) 0 (0, n-1) (concatMap (uncurry calcItemUsers) items)

calcItemUsers :: [(Int, Double)] -> Double -> [(Int, Double)]
calcItemUsers us p =
    map (second (* pp)) us
  where
    pp = p / sum (map snd us)

formatStats :: [(UserHandle, Double, Double)] -> Text
formatStats = T.unlines . map (uncurry3 formatStat)

formatStat :: UserHandle -> Double -> Double -> Text
formatStat user p u | p > u = [i|@#{user} payed #{p} consumed #{u}, is owed #{p - u}|]
formatStat user p u | otherwise = [i|@#{user} payed #{p} consumed #{u}, owes #{u - p}|]

allUserIds :: ChatM [Int]
allUserIds = do
    nUsers <- gets $ length . csUsers
    return $ enumFromTo 0 (nUsers - 1)

addItem :: Item -> ChatState -> ChatState
addItem n cs = cs { csItems = newItems }
    where newItems = n : csItems cs

getUserId :: Text -> ChatM Int
getUserId u = do 
    userId <- gets $ elemIndex u . csUsers
    maybe (throwError $ [i|"User @#{u} isn't participating"|]) return userId


    
