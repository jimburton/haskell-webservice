{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           System.IO
import           Data.Time.Format
import           System.Random
import           Data.List.Split
import           Data.Char
import           Data.Dates

data WeatherField = WeatherField T.Text Float deriving (Show)

instance FromRow WeatherField where
  fromRow = WeatherField <$> field <*> field 

instance ToRow WeatherField where
  toRow (WeatherField theDate temp) = toRow (theDate, temp)

main :: IO ()
main = do
  conn <- open "data/np-weather.db"
  execute_ conn "DROP TABLE IF EXISTS weather"
  execute_ conn "CREATE TABLE IF NOT EXISTS weather (id INTEGER PRIMARY KEY, the_date TEXT, temperature REAL)"
  populateDB conn
  r <- query_ conn "SELECT the_date, temperature FROM weather" :: IO [WeatherField]
  mapM_ print r
  close conn

populateDB conn = do
  ls <- fmap lines $ readFile "data/weather.txt"
  mapM_ (\l -> do let (x:xs) = splitOn "]" (tail l)
                      num = (read (dropWhile isSpace (concat xs)))::Float
                  execute conn "INSERT INTO weather (the_date, temperature) VALUES (?,?)" (WeatherField (T.pack x) num)) ls
        

makeFile = do
  let nyd = DateTime {year=2017, month=1, day=1, hour=0, minute=0, second=0}
  h <- openFile "data/weather.txt" WriteMode
  g <- getStdGen
  _ <- writeToFile nyd h g
  hClose h

writeToFile :: DateTime -> Handle -> StdGen -> IO ()
writeToFile d h g = if d <= DateTime {year=2017, month=12, day=31, hour=0, minute=0, second=0} 
                    then do let (num, g') = randomR ((-40), 40) g
                                date = showDateTime d
                            hPutStrLn h ("[" ++  date ++ "] " ++ (show (num ::Float)))
                            writeToFile (addInterval d (Days 1)) h g'
                    else return ()
showDateTime :: DateTime -> String
showDateTime (DateTime y m d _ _ _) = (pad y) ++ "-" ++ (pad m) ++ "-" ++ (pad d)
  where pad d = if d < 10 then '0' : (show d) else show d
