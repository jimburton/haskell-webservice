{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies, QuasiQuotes, TemplateHaskell, DeriveGeneric #-}
module WeatherService.Service (WeatherField(..)
                              , dayHandler
                              , dayPutHandler
                              , rangeHandler
                              , maxRangeHandler
                              , aboveTempHandler
                              , methodNotAllowedHandler) where
{-| Semester 2 assignment for CI285, University of Brighton
    Jim Burton <j.burton@brighton.ac.uk>
-}
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
import           Web.Routes
import           Web.Routes.TH
import           Web.Routes.Happstack
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Data.Aeson
import           GHC.Generics                  (Generic)
import qualified Data.ByteString.Lazy.Char8 as BC

import WeatherService.Sitemap

data WeatherField = WeatherField {date :: Text, temperature :: Float}
                    deriving (Generic, Show)

instance FromRow WeatherField where -- ^ Marshal data from DB to our ADT
  fromRow = WeatherField <$> field <*> field

instance ToRow WeatherField where -- ^ Marshal data from our ADT to the DB
  toRow (WeatherField theDate temp) = toRow (theDate, temp)

instance ToJSON   WeatherField -- ^ Marshal data from our ADT to JSON
instance FromJSON WeatherField -- ^ Marshal data from JSON to our ADT


{-| Handle requests for a single date. -}
dayHandler :: Text -> Connection -> RouteT Sitemap (ServerPartT IO) Response
dayHandler d conn = do
  r <- liftIO (queryNamed conn "SELECT the_date, temperature \
                               \ FROM  weather \
                               \ WHERE the_date = :dt" [":dt" := d] :: IO [WeatherField])
  liftIO $ debugM "Date Query" (listToOutput r)
  case r of
    [] -> notFoundHandler
    _  -> ok $ toResponse (listToOutput r)

{-| Handle PUT requests for date/temperature pairs. -}
dayPutHandler :: Text -> Text -> Connection -> RouteT Sitemap (ServerPartT IO) Response
dayPutHandler d t conn = do
  r <- liftIO (queryNamed conn "SELECT the_date, temperature \
                               \ FROM  weather \
                               \ WHERE the_date = :dt" [":dt" := d] :: IO [WeatherField])
  liftIO $ debugM "Date PUT request" (listToOutput r)
  case r of
    [] -> insertHandler d t conn
    _ -> updateHandler d t conn


{-| Insert a new date/temperature pair. -}
insertHandler :: Text -> Text -> Connection -> RouteT Sitemap (ServerPartT IO) Response
insertHandler d t conn = do
  let t' = (read $ unpack t)::Float
  liftIO (execute conn "INSERT INTO weather (the_date, temperature) VALUES (?,?)" (WeatherField d t'))
  ok $ emptyJSONResponse

{-| Update a date/temperature pair. -}
updateHandler :: Text -> Text -> Connection -> RouteT Sitemap (ServerPartT IO) Response
updateHandler d t conn = do
  let t' = (read $ unpack t)::Float
  liftIO (executeNamed conn "UPDATE weather SET temperature = :t WHERE the_date = :d"
           [":t" := t, ":d" := d])
  ok $ emptyJSONResponse

{-| Task One: Range Handler -}
rangeHandler :: Text -> Text -> Connection -> RouteT Sitemap (ServerPartT IO) Response
rangeHandler d1 d2 conn = do
  r <- liftIO (queryNamed conn "SELECT the_date, temperature \
                               \ FROM weather \
                               \ WHERE the_date BETWEEN :d1 AND :d2" [":d1" := d1, ":d2" := d2] :: IO [WeatherField])
  liftIO $ debugM "Date Range GET Request" (listToOutput r)
  case r of
    [] -> notFoundHandler
    _  -> ok $ toResponse (listToOutput r)

{-| Task Two: Max Range Handler |-}
maxRangeHandler :: Text -> Text -> Connection -> RouteT Sitemap (ServerPartT IO) Response
maxRangeHandler d1 d2 conn = do
  r <- liftIO (queryNamed conn "SELECT the_date, MAX(temperature) \
                               \ FROM weather \
                               \ WHERE the_date BETWEEN :d1 AND :d2" [":d1" := d1, ":d2" := d2] :: IO [WeatherField])
  liftIO $ debugM "Max Date Range GET Request" (listToOutput r)
  case r of
    [] -> notFoundHandler
    _  -> ok $ toResponse (listToOutput r)

{-| Task Three: Temperature Greater Than Handler |-}
aboveTempHandler :: Text -> Connection -> RouteT Sitemap (ServerPartT IO) Response
aboveTempHandler t conn = do
  r <- liftIO (queryNamed conn "SELECT the_date, temperature \
                               \ FROM weather \
                               \ WHERE temperature >= :t" [":t" := t] :: IO [WeatherField])
  liftIO $ debugM "Greater Than Temp GET Request" (listToOutput r)
  case r of
    [] -> notFoundHandler
    _  -> ok $ toResponse (listToOutput r)


{-| Turn a list of WeatherFields into a JSON object. -}
listToOutput :: ToJSON a => [a] -> String
listToOutput xs = "[" ++ intercalate "," (map (BC.unpack . encode) xs) ++ "]"

{-| Return 404 Not Found and an empty JSON object -}
notFoundHandler :: RouteT Sitemap (ServerPartT IO) Response
notFoundHandler = notFound emptyJSONResponse

{-| Return 405 Method Not Allowed (Task Three) -}
methodNotAllowedHandler :: RouteT Sitemap (ServerPartT IO) Response
methodNotAllowedHandler = resp 405 emptyJSONResponse

{-| Return an empty JSON response -}
emptyJSONResponse :: Response
emptyJSONResponse = toResponse (pack "[]")
