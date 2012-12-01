{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Text (Text)
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

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

transcriptRequest :: String -> String -> String -> IO (Request m)
transcriptRequest subdomain room token = do
  request <- parseUrl $ "https://" ++ subdomain ++ ".campfirenow.com/room/" ++ room ++ "/transcript.json"
  let request' = applyBasicAuth (B.pack token) "x" request
  return request'

loadJSON :: IO BL.ByteString
loadJSON = do
  request <- transcriptRequest "paperlesspost" "203957" "470ad468ec9a68996be2812d5367c97f2c87e545"
  response <- withManager $ httpLbs request
  return $ responseBody response

parseTranscript = do
  transcriptBody <- loadJSON
  return $ (decode transcriptBody :: Maybe Transcript)

writeLog :: Transcript -> IO ()
writeLog transcript = undefined


main = do
  [subdomain, room, token] <- getArgs
  -- make request object, make http request, parse json
  parseTranscript >>= print
