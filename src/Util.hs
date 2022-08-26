module Util where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Monad.IO.Class   

bark :: MonadIO m => [T.Text] -> m ()
bark = liftIO . T.putStrLn . T.concat
