{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Tests where

import Data.Aeson
import qualified Data.ByteString.Lazy as Lazy
import Test.HUnit as HUnit
import Main
import Weather
import Joke
import Poetry
import Canvas
import Data.Tree

test1 = TestCase (assertEqual "decoding weather" (Just (Weather "clear sky" 281.49 "01d")) (decode "{\"coord\":{\"lon\":17.75,\"lat\":60},\"weather\":[{\"id\":800,\"main\":\"Clear\",\"description\":\"clear sky\",\"icon\":\"01d\"}],\"base\":\"stations\",\"main\":{\"temp\":281.49,\"feels_like\":275.19,\"temp_min\":281.15,\"temp_max\":282.04,\"pressure\":1029,\"humidity\":46},\"visibility\":10000,\"wind\":{\"speed\":5.66,\"deg\":320},\"clouds\":{\"all\":0},\"dt\":1614696833,\"sys\":{\"type\":1,\"id\":1731,\"country\":\"SE\",\"sunrise\":1614663878,\"sunset\":1614701879},\"timezone\":3600,\"id\":2666218,\"name\":\"Uppsala County\",\"cod\":200}"))
test2 = TestCase (assertEqual "decoding joke" (Just (Joke "My dog used to chase people on a bike a lot. It got so bad I had to take his bike away.")) (decode "{\r\n  \"id\": \"R7UfaahVfFd\",\r\n  \"joke\": \"My dog used to chase people on a bike a lot. It got so bad I had to take his bike away.\",\r\n  \"status\": 200\r\n}"))
test3 = TestCase (assertEqual "decoding poetry" (Just (Poetry "Oh, They have Robbed Me of The Hope" "Anne Bronte" ["Oh, they have robbed me of the hope","My spirit held so dear;","They will not let me hear that voice","My soul delights to hear.","They will not let me see that face","I so delight to see;","And they have taken all thy smiles,","And all thy love from me.","","Well, let them seize on all they can: --","One treasure still is mine, --","A heart that loves to think on thee,","And feels the worth of thine."] )) (decode "{\r\n        \"title\": \"Oh, They have Robbed Me of The Hope\",\r\n        \"author\": \"Anne Bronte\",\r\n        \"lines\": [\r\n            \"Oh, they have robbed me of the hope\",\r\n            \"My spirit held so dear;\",\r\n            \"They will not let me hear that voice\",\r\n            \"My soul delights to hear.\",\r\n            \"They will not let me see that face\",\r\n            \"I so delight to see;\",\r\n            \"And they have taken all thy smiles,\",\r\n            \"And all thy love from me.\",\r\n            \"\",\r\n            \"Well, let them seize on all they can: --\",\r\n            \"One treasure still is mine, --\",\r\n            \"A heart that loves to think on thee,\",\r\n            \"And feels the worth of thine.\"\r\n        ],\r\n        \"linecount\": \"12\"\r\n    }"))
test_filestructure = TestCase (assertEqual "building foldersstructure" (Folder "1" "root" Nothing [Folder "2" "root child" (Just "1") [] []] [File "101" "file1" "www.file.com" "1"]) (fileStructure [(Folder "1" "root" Nothing [] []),(Folder "2" "root child" (Just "1") [] [])] [(File "101" "file1" "www.file.com" "1")]))
test_sortfolders = TestCase (assertEqual "sorting folders" (Folder "1" "root" Nothing [Folder "2" "root child" (Just "1") [] []] []) (sortFolders [(Folder "1" "root" Nothing [] []),(Folder "2" "root child" (Just "1") [] [])]) )
test_addfiles = TestCase (assertEqual "adding files" ([(Folder "1" "root" Nothing [] [(File "101" "file1" "www.file.com" "1")]),(Folder "2" "root child" (Just "1") [] [])]) (addFiles [(Folder "1" "root" Nothing [] []),(Folder "2" "root child" (Just "1") [])] [(File "101" "file1" "www.file.com" "1")]))
test_foldertotree = TestCase (assertEqual "folder to tree" (Node "root" [Node "root child" []]) (folderToTree (Folder "1" "root" Nothing [Folder "2" "root child" (Just "1") [] []] [])))
test_folderstotrees = TestCase (assertEqual "folders to trees" ([Node "root" [],Node "root child" []]) (foldersToTrees [(Folder "1" "root" Nothing [] []),(Folder "2" "root child" (Just "1") [] [])]))
test_filetotree = TestCase (assertEqual "file to tree" (Node "[example.png](www.example.org)" []) (fileToTree (File "508" "example.png" "www.example.org" "1")))

tests = TestList [test1,test2,test3,test_filestructure,test_sortfolders,test_addfiles,test_foldertotree,test_folderstotrees,test_filetotree]

runTests = runTestTT tests



