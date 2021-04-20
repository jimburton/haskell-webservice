{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module WeatherService.Service (WeatherField(..)
                              , dayHandler
                              , dayPutHandler) where

import           System.Log.Logger ( updateGlobalLogger
                                   , rootLoggerName
                                   , setLevel
                                   , debugM
                                   , Priority(..)
                                   )
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad          (msum)
import           Data.List         (intercalate)
import           Data.Text         (Text, pack, unpack)
import           Happstack.Server  
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Data.Aeson
import           GHC.Generics                  (Generic)
import qualified Data.ByteString.Lazy.Char8 as BC

data WeatherField = WeatherField {date :: Text, temperature :: Float}
                    deriving (Generic, Show)

-- | Marshal data from DB to our ADT
instance FromRow WeatherField where 
  fromRow = WeatherField <$> field <*> field 

-- | Marshal data from our ADT to the DB
instance ToRow WeatherField where 
  toRow (WeatherField theDate temp) = toRow (theDate, temp)

-- | Marshal data from our ADT to JSON
instance ToJSON   WeatherField
-- | Marshal data from JSON to our ADT
instance FromJSON WeatherField 

{-| Handle reuests for a single date. -}
dayHandler :: Text -> Connection -> ServerPart Response
dayHandler d conn = do
  r <- liftIO (queryNamed conn "SELECT the_date, temperature \
                               \ FROM  weather \
                               \ WHERE the_date = :dt" [":dt" := d] :: IO [WeatherField])
  liftIO $ debugM "Date Query" (listToOutput r) -- NB example of how to output debug messages
  case r of
    [] -> notFoundHandler
    _  -> ok $ toResponse (listToOutput r)

{-| Handle PUT requests for date/temperature pairs. -}
dayPutHandler :: Text -> Text -> Connection -> ServerPart Response
dayPutHandler d t conn = do
  r <- liftIO (queryNamed conn "SELECT the_date, temperature \
                               \ FROM  weather \
                               \ WHERE the_date = :dt" [":dt" := d] :: IO [WeatherField])
  liftIO $ debugM "Date PUT request" (listToOutput r)
  case r of
    [] -> insertHandler d t conn
    _  -> updateHandler d t conn

{-| Insert a new date/temperature pair. -}
insertHandler :: Text -> Text -> Connection -> ServerPart Response
insertHandler d t conn = do
  let t' = (read $ unpack t)::Float
  liftIO (execute conn "INSERT INTO weather (the_date, temperature) VALUES (?,?)" (WeatherField d t'))
  ok emptyJSONResponse

{-| Update a date/temperature pair. -}
updateHandler :: Text -> Text -> Connection -> ServerPart Response
updateHandler d t conn = do
  let t' = (read $ unpack t)::Float
  liftIO (executeNamed conn "UPDATE weather SET temperature = :t WHERE the_date = :d"
           [":t" := t, ":d" := d])
  ok emptyJSONResponse

{-| Return 404 Not Found and an empty JSON object -}
notFoundHandler :: ServerPart Response
notFoundHandler = notFound emptyJSONResponse

{-| An empty JSON object -}
--emptyJSONObject :: ServerPart Response
emptyJSONResponse = toResponse (pack "[]")

{-| Turn a list of WeatherFields into a JSON object. -}
listToOutput :: ToJSON a => [a] -> String
listToOutput xs = "[" ++ intercalate "," (map (BC.unpack . encode) xs) ++ "]"

