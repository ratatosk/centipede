module Types where

import Control.Monad.State.Lazy
import Data.Text (Text)
import Data.HashMap.Lazy

import Servant.Client (ClientM, ClientEnv, ClientError, runClientM)
import Telegram.Bot.API.Types (ChatId)


data Item = Item
    { iName   :: Text 
    , iPayers :: [(Int, Double)]
    , iUsers  :: [(Int, Double)]
    }

data ChatState = ChatState
    { csUsers  :: [(Text, Int)]
    , csGroups :: [[Int]]
    , csItems  :: [Item]
    }

type BotState = HashMap ChatId ChatState

type BotM a = StateT BotState ClientM a

runBotM :: BotM a -> ClientEnv -> IO (Either ClientError a)
runBotM b e = runClientM (evalStateT b empty) e

-- type ChatM a = StateT ChatState ClientM a

-- inChat :: ChatId -> (ChatState -> BotM a) -> BotMa
-- inChat id act = do
--     <- 