module Main where

import Test.Tasty (testGroup)
import Test.HTTP
import Data.List (isInfixOf)

url = "http://localhost:8000/weather/"

day0 = "{\"date\":\"2017-01-01\",\"temperature\":-23.164497}"

testDayQuery = httpTestCase "Weather Service Day Query" url $ do
    landing <- get "date/2017-01-01"
    assert "Includes initial data" $ day0 `isInfixOf` landing
    debug landing

testRangeQuery = httpTestCase "Weather Service Range Query" url $ do
    landing <- get "range/2017-01-01/2017-01-02"
    assert "Includes initial data" $ day0 `isInfixOf` landing
    debug landing

tests = testGroup "tests" [testDayQuery, testRangeQuery]

main = defaultMain tests
