{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies, QuasiQuotes, TemplateHaskell, DeriveGeneric #-}
module Main where

import           System.Log.Logger ( updateGlobalLogger
                                   , rootLoggerName
                                   , setLevel
                                   , debugM
                                   , Priority(..)
                                   )

import Prelude                 hiding (head)
import                         Control.Monad.IO.Class (liftIO)
import Control.Monad           (msum)
import Data.Data               (Data, Typeable)
import Data.Monoid             (mconcat)
import Data.Text               (Text, pack, unpack)
import Data.Aeson
import Data.List               (intercalate)
import Happstack.Server
import Database.SQLite.Simple
import qualified Data.ByteString.Lazy.Char8 as BC

import WeatherService.Sitemap
import WeatherService.Service

main :: IO()
main = do
  updateGlobalLogger rootLoggerName (setLevel INFO) -- change level to DEBUG for testing
  conn <- open "data/np-weather.db"
  simpleHTTP nullConf $ do
    setHeaderM "Content-Type" "application/json"
    msum [
      dirs "weather/date" $ do method [GET, POST]
                               path $ \d -> dayHandler d conn
      , dirs "weather/date" $ do method PUT
                                 path $ \d -> path $ \t -> dayPutHandler d t conn
      , dirs "weather/range" $ do method GET
                                  path $ \d1 -> path $ \d2 -> rangeHandler d1 d2 conn
      , dirs "weather/max" $ do method GET
                                path $ \d1 -> path $ \d2 -> maxRangeHandler d1 d2 conn
      , dirs "weather/above" $ msum [do method GET
                                        path $ \t -> aboveTempHandler t conn
                                    , do method (not . (==) GET)
                                         methodNotAllowedHandler]
      ]


