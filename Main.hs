{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BL

data Message = Message {
  _type :: Text,
  user_id :: Maybe Integer,
  body :: Maybe Text } deriving (Show)

data Transcript = Transcript {
  messages :: [Message] } deriving (Show)

instance FromJSON Transcript where
  parseJSON (Object v) = do
    messages <- v .: "messages"
    return $ Transcript messages

instance FromJSON Message where
  parseJSON (Object v) = do
    body    <- v .:? "body"
    user_id <- v .:  "user_id"
    _type   <- v .:  "type"
    return $ Message _type user_id body

loadJSON :: IO (BL.ByteString)
loadJSON = do
  args <- getArgs
  transcript <- BL.readFile (head args)
  return transcript

main :: IO ()
main = do
  transcript <- loadJSON
  let req = decode transcript :: Maybe Transcript
  print req
