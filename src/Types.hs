module Types where

import Control.Monad.State.Strict
import Data.Text (Text)
import Data.HashMap.Strict

import Servant.Client (ClientM, ClientEnv, ClientError, runClientM)
import Telegram.Bot.API.Types (ChatId)


data Item = Item
    { iName   :: Text 
    , iPayers :: [(Int, Double)]
    , iUsers  :: [(Int, Double)]
    }

data ChatState = ChatState
    { csUsers  :: [UserHandle]
    , csGroups :: [[Int]]   
    , csItems  :: [Item]
    }

type UserHandle = Text

data Ctx = Ctx
    { ctxUser   :: UserHandle
    , ctxChatId :: ChatId
    } deriving (Show)

type BotState = HashMap ChatId ChatState

type BotM a = StateT BotState ClientM a

runBotM :: BotM a -> ClientEnv -> IO (Either ClientError a)
runBotM b e = runClientM (evalStateT b empty) e