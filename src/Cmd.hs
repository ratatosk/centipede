module Cmd 
    ( Cmd(..)
    , parseCmd
    , processCmd
    ) where

import Control.Monad
import Data.Either.Extra
import Text.Parsec
import Text.Parsec.Text

import Data.Text

import Types


data Cmd
    = Add Text Double Text
    | Stat
    deriving (Show)

spaces1 :: Parser ()
spaces1 = skipMany1 space

double :: Parser Double
double = do
    int <- many1 digit
    frac <- option "" $ liftM2 (:) (char '.') (many digit)
    return $ read (int ++ frac)

addParser :: Parser Cmd
addParser = do
    string "add" >> spaces1
    item <- pack <$> (many1 $ satisfy (/= ':'))
    spaces >> char ':' >> spaces
    paid <- double
    spaces  
    user <- pack <$> (char '@' >> many1 alphaNum)
    return $ Add item paid user

statParser :: Parser Cmd
statParser = string "stat" >> return Stat

cmdParser :: Parser Cmd
cmdParser = char '/' *> (addParser <|> statParser) <* eof
    
parseCmd :: Text -> Either String Cmd
parseCmd = mapLeft show . parse cmdParser ""

processCmd :: Cmd -> BotM Text
processCmd cmd = return $ pack $ "Understood: " ++ show cmd
