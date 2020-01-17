{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies, QuasiQuotes, TemplateHaskell, DeriveGeneric #-}
module Main where
{-| Semester 2 assignment for CI285, University of Brighton
    Jim Burton <j.burton@brighton.ac.uk>
-}
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
import Web.Routes              ( PathInfo(..), RouteT, showURL
                               , runRouteT, Site(..), setDefault, mkSitePI)
import Web.Routes.TH           (derivePathInfo)
import Web.Routes.Happstack    (implSite)
import Database.SQLite.Simple
import GHC.Generics            (Generic)
import qualified Data.ByteString.Lazy.Char8 as BC

import WeatherService.Sitemap
import WeatherService.Service

$(derivePathInfo ''Sitemap)

route :: Connection -> Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route conn url =
    case url of
      (Date d)       -> do method GET
                           dayHandler d conn
      (Update d t)   -> do method PUT
                           dayPutHandler d t conn
      (Range d1 d2)  -> do method GET
                           rangeHandler d1 d2 conn
      (Max d1 d2)    -> do method GET
                           maxRangeHandler d1 d2 conn
      (Above t)      -> msum [do method GET
                                 aboveTempHandler t conn,
                              do method (not . (==) GET)
                                 methodNotAllowedHandler]

site :: Connection -> Site Sitemap (ServerPartT IO Response)
site conn = mkSitePI (runRouteT $ route conn)

main :: IO()
main = do
  updateGlobalLogger rootLoggerName (setLevel INFO) -- change level to DEBUG for testing
  conn <- open "data/np-weather.db"
  simpleHTTP nullConf $ do
    setHeaderM "Content-Type" "application/json"
    msum [ implSite "http://localhost:8000" "/weather" $ site conn]

