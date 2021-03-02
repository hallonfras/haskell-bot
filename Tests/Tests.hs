{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Tests where

import Data.Aeson
import qualified Data.ByteString.Lazy as Lazy
import Test.HUnit as HUnit
import Main
import Weather
import Joke
import Poetry

test1 = TestCase (assertEqual "decoding weather" (Just (Weather "clear sky" 281.49 "01d")) (decode "{\"coord\":{\"lon\":17.75,\"lat\":60},\"weather\":[{\"id\":800,\"main\":\"Clear\",\"description\":\"clear sky\",\"icon\":\"01d\"}],\"base\":\"stations\",\"main\":{\"temp\":281.49,\"feels_like\":275.19,\"temp_min\":281.15,\"temp_max\":282.04,\"pressure\":1029,\"humidity\":46},\"visibility\":10000,\"wind\":{\"speed\":5.66,\"deg\":320},\"clouds\":{\"all\":0},\"dt\":1614696833,\"sys\":{\"type\":1,\"id\":1731,\"country\":\"SE\",\"sunrise\":1614663878,\"sunset\":1614701879},\"timezone\":3600,\"id\":2666218,\"name\":\"Uppsala County\",\"cod\":200}"))
