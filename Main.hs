{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import System.Environment (getArgs)
import System.IO
import Control.Applicative ((<$>), (<*>), empty)
import Data.Maybe
import Control.Failure
import Data.Aeson
import Data.DateTime
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

data Message = Message {
  _type :: String,
  user_id :: Maybe Integer,
  body :: Maybe String } deriving (Show)

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

data TranscriptRequest = TranscriptRequest {
  subdomain :: String,
  room :: String,
  token :: String } deriving (Show)

buildTranscriptRequest :: Failure HttpException m => String -> String -> String -> m (Request m1)
buildTranscriptRequest subdomain room token = do
  request <- parseUrl $ "https://" ++ subdomain ++ ".campfirenow.com/room/" ++ room ++ "/transcript.json"
  let request' = applyBasicAuth (B.pack token) "x" request
  return request'

makeTranscriptRequest request = do
  response <- withManager $ httpLbs request
  return $ responseBody response

-- makeTranscriptRequest req = BL.readFile "transcript.json"

parseTranscript :: BL.ByteString -> Maybe Transcript
parseTranscript transcriptBody = decode transcriptBody

writeLog transcript = do
  withFile "output.log" WriteMode (\handle -> do
    hPutStr handle (foldl (\acc m -> acc ++ logLine m) "" (messages transcript)))

logLine :: Message -> String
logLine (Message "TextMessage" user_id body) = "<" ++ show (fromJust user_id) ++ "> " ++ fromJust body ++ "\n"
logLine (Message "KickMessage" user_id body) = show (fromJust user_id) ++ " has left #campfire" ++ "\n"
logLine (Message "EnterMessage" user_id body) = show (fromJust user_id) ++ " has joined #campfire" ++ "\n"
logLine (Message "TopicChangeMessage" user_id body) = show (fromJust user_id) ++ " changed the topic of #campfire to: " ++ fromJust body ++ "\n"
logLine _ = ""

main = do
  [subdomain, room, token] <- getArgs
  request <- buildTranscriptRequest subdomain room token
  json <- makeTranscriptRequest request
  let transcript = parseTranscript json
  writeLog $ fromJust transcript
