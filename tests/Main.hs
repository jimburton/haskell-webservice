{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty (testGroup)
import Test.HTTP
import Data.List (isInfixOf)
import Data.Aeson (decode)
import WeatherService.Service
import qualified Data.ByteString.Lazy.Char8 as LB

url = "http://localhost:8000/weather/"

day0 = "{\"temperature\":-23.164497,\"date\":\"2017-01-01\"}"
         
hottestDay = "[{\"date\":\"2017-09-15\",\"temperature\":39.502533}]"

testDayQuery = httpTestCase "Weather Service Day Query" url $ do
    landing <- get "date/2017-01-01"
    assert "Includes initial data" $ day0 `isInfixOf` landing
    --debug landing

testRangeQuery = httpTestCase "Weather Service Range Query" url $ do
    landing <- get "range/2017-01-01/2017-12-31" --should be 365 records
    let days = decode (LB.pack landing) :: Maybe [WeatherField]
    case days of
      Nothing   -> assert "Has year of records" False
      (Just ms) -> assert "Has year of records" $ length ms == 365

testMaxQuery = httpTestCase "Weather Service Max Query" url $ do
    landing <- get "max/2017-01-01/2017-12-31"
    assert "Has max record" $ landing == hottestDay

testAboveQuery = httpTestCase "Weather Service Above Query" url $ do
    landing <- get "above/39" --should be two records
    let days = decode (LB.pack landing) :: Maybe [WeatherField]
    debug (show days)
    case days of
      Nothing   -> assert "Has Above record" False
      (Just ms) -> assert "Has Above record" $ length ms == 2

tests = testGroup "tests" [testDayQuery, testRangeQuery, testMaxQuery, testAboveQuery]

main = defaultMain tests
