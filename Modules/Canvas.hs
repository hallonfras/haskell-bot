{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric   #-}

module Canvas where
import Data.Data
import Data.List
import           Data.Aeson
import Data.Aeson.Types
import           Data.String.Builder
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as BSL
import           Network.HTTP.Simple
import qualified Language.Haskell.Interpreter as Hint
import Discord
import Discord.Types
import Utils
import Data.List.Split as Split
import Data.Text (isPrefixOf, toLower, Text, unpack, pack, isInfixOf, splitAt, splitOn)

instance Stringable Course where
    stringIt c = (course_name c) ++ "\nID: " ++ (course_id c) ++ "\nCODE: " ++ (course_code c)

instance Stringable Assignment where
    stringIt a = (assignment_name a) ++ "\nID: " ++ (assignment_id a) ++ "\nPOINTS: " ++ (points_possible a) ++ "\nURL: " ++ (assignment_url a) ++ "\nCREATION DATE: " ++ (created_at a)

data Folder = Folder {
    folder_id :: String,
    folder_name :: String, 
    parent_id :: Maybe String, --root folder has no parent_id
    children :: [Folder],
    files :: [File]
}
data File = File {
    file_id :: String,
    file_name :: String,
    file_url :: String
} deriving (Show,Eq)

data Course = Course { 
  course_id   :: String
, course_name :: String
, course_code :: String
} deriving (Data,Typeable,Show)

data Assignment = Assignment {
    assignment_id :: String,
    assignment_name :: String,
    points_possible :: String,
    assignment_url :: String,
    created_at :: String
} deriving (Data,Typeable,Show)

instance Eq Folder where
    (==) f1 f2 = folder_id f1 == folder_id f2 && 
                                folder_name f1 == folder_name f2 && 
                                parent_id f1 == parent_id f2 && 
                                children f1 == children f2 && 
                                files f1 == files f2

instance Show Folder where
    show f = " {fid: " ++ show (folder_id f) ++ " fname: " ++ show (folder_name f) ++ " fpid: " ++ show (parent_id f) ++ " children: " ++ show (children f) ++ " files: " ++ show (files f) ++ "} "


instance FromJSON Folder where
    parseJSON (Object v) = do
        id <- v .: "id" 
        name <- v .: "name"
        parent <- v .:? "parent_folder_id" -- .:? takes into account for Null and nonexisting keys
        return $ Folder (show (id ::Int)) name (maybeToString (parent :: Maybe Int)) [] [] --new folder does not have children nor files
    parseJSON _ = mempty

instance FromJSON Course where
    parseJSON (Object v) = do
        id <- v .: "id" 
        name <- v .: "name"
        code <- v .: "course_code"
        return (Course {course_id = (show (id :: Int)),course_name= name,course_code= code})
    parseJSON _ = mempty

instance FromJSON Assignment where
    parseJSON (Object v) = do
        id <- v .: "id" 
        name <- v .: "name"
        points <- v .: "points_possible"
        url <- v .: "html_url"
        created_at <- v .: "created_at"
        return $ Assignment (show (id :: Int)) name (show (points :: Double)) url created_at
    parseJSON _ = mempty

--api request to json response
--lägg till felhantering

apiRequest :: String -> [Char] -> DiscordHandler (S8.ByteString)
apiRequest source token = do
    request <- parseRequest source
    let request' = setRequestMethod "GET"
                    $ setRequestHeader "Authorization" [S8.pack ("Bearer " ++ token)]
                    $ request
    response <- httpBS request'
    return (getResponseBody response)

getCourses :: String -> DiscordHandler (MessageData [Course])
getCourses token = do
    json <- apiRequest "https://uppsala.instructure.com/api/v1/users/self/courses" token
    jsonToMessageData json

-- https://uppsala.instructure.com/api/v1/courses/26396
getAssignments :: String -> String -> DiscordHandler (MessageData [Assignment])
getAssignments token courseid = do
    json <- apiRequest ("https://uppsala.instructure.com/api/v1/courses/" ++ courseid ++ "/assignments") token
    jsonToMessageData json

apiRequest' :: String -> [Char] -> IO (S8.ByteString)
apiRequest' source token = do
    request <- parseRequest source
    let request' = setRequestMethod "GET"
                    $ setRequestHeader "Authorization" [S8.pack ("Bearer " ++ token)]
                    $ request
    response <- httpBS request'
    return (getResponseBody response)
getFolders :: String -> String -> IO ([Folder])
getFolders token courseid = do
    json <- apiRequest' ("https://uppsala.instructure.com/api/v1/courses/" ++ courseid ++ "/folders") token
    let value = Data.Aeson.decode $ BSL.fromStrict json
    return (fromMaybe value)

--rename the local functions so it becomes more clear
sortFolders :: [Folder] -> Folder
sortFolders folderList = findRoot folderList folderList
    where
        findRoot (f:fs) folders = -- searches for root folder
            if parent_id f == Nothing then buildFolder (delete f folders) f -- when root folder is found, calls aux function and removes root from golders
            else findRoot fs folders
            where
                --builds folder structure from root to bottom
                buildFolder [] root = root
                buildFolder (x:xs) root@(Folder r_id r_name r_pid children files)
                    | fromMaybe (parent_id x) == r_id = buildFolder xs (Folder r_id r_name r_pid ((buildFolder (delete f folders) x):children) files) --if f has parent_id equal to root iq then add (f with its children) to children 
                    | otherwise = buildFolder xs root

canvAssignments :: Message -> DiscordHandler ()
canvAssignments m = do
        let
                args = tail $ Split.splitOn "-" (removeSpace $ unpack $ messageText m)
                courseid = args !! 0 -- courseid is the first and only argument ex. (!assignments -3085)
                title = pack "Assignments"
                icon = pack "https://static.thenounproject.com/png/51139-200.png"
<<<<<<< HEAD
        msgdata <- getAssignments "canvas token" courseid
=======
        msgdata <- getAssignments "14589~soDe3Fvwq2zzG4ab8zqPOS7CcJIKsPSybnHE0sPjF7vFTEdGn2eoKaHN9VTUrYqy" courseid
>>>>>>> feature-canvas
        handleMessage m msgdata title icon

canvCourses :: Message -> DiscordHandler ()
canvCourses m = do
        let title = pack "Courses"
            icon = pack "https://cdn2.iconfinder.com/data/icons/online-university/96/computer_training_art_course-512.png"
<<<<<<< HEAD
        msgdata <- getCourses "canvas token"
=======
        msgdata <- getCourses "14589~soDe3Fvwq2zzG4ab8zqPOS7CcJIKsPSybnHE0sPjF7vFTEdGn2eoKaHN9VTUrYqy"
>>>>>>> feature-canvas
        handleMessage m msgdata title icon