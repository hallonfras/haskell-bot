{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric   #-}

module Canvas where
import Data.Data

import Data.Maybe
import Data.List
import System.Environment
import           Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as BSL

import Discord
import Discord.Types
import Utils
import Data.List.Split as Split
import Data.Text (isPrefixOf, toLower, Text, unpack, pack, isInfixOf, splitAt, splitOn)
import Data.Tree
import qualified TreePrint

instance Stringable Course where
    stringIt (Course id name code) = name ++ "\nID: " ++ id ++ "\nCODE: " ++ code

instance Stringable Assignment where
    stringIt (Assignment id name points url creation) = name ++ "\nID: " ++ id ++ "\nPOINTS: " ++ points ++ "\nURL: " ++ url ++ "\nCREATION DATE: " ++ creation

instance Stringable Folder where
    stringIt folder = TreePrint.treeDraw $ folderToTree $ folder


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
    file_url :: String,
    file_folder_id :: String
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

instance FromJSON File where
    parseJSON (Object v) = do
        id <- v .: "id"
        name <- v .: "display_name"
        url <- v .: "url"
        folderid <- v .: "folder_id"
        return $ File (show (id :: Int)) name url (show (folderid :: Int))
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
{- apiRequest source token
     
     SIDE EFFECTS:
     RETURNS: the concatenation of cool and nice
     EXAMPLES: coolNiceFunc "cool" "nice" = "coolnice"
  -}

----------------------------------------------------------------------
getCourses :: String -> DiscordHandler (MessageData (Maybe [Course]))
getCourses token = do
    json <- apiRequest "https://uppsala.instructure.com/api/v1/users/self/courses" token
    let courses = Data.Aeson.decode $ BSL.fromStrict json
    toMessageData courses

-- https://uppsala.instructure.com/api/v1/courses/26396
getAssignments :: String -> String -> DiscordHandler (MessageData (Maybe [Assignment]))
getAssignments token courseid = do
    json <- apiRequest ("https://uppsala.instructure.com/api/v1/courses/" ++ courseid ++ "/assignments") token
    let assignments = Data.Aeson.decode $ BSL.fromStrict json
    toMessageData assignments

getFileStructure :: String -> String -> DiscordHandler (MessageData (Maybe Folder))
getFileStructure token courseid = do
    folders <- getFolders token courseid
    files <- getFiles token courseid
    if isNothing folders || isNothing files
    then do toMessageData Nothing
    else do 
     let folder = fileStructure (Utils.fromMaybe folders) (Utils.fromMaybe files)
     toMessageData (Just folder)


{-
---------------------------------------------------
START OF CANVAS DISCORD MESSAGE FUNCTIONS
---------------------------------------------------
-}

canvFiles :: Message -> DiscordHandler ()
canvFiles m = do
    let
        args = tail $ Split.splitOn "-" (removeSpace $ unpack $ messageText m) --gets all arguments
        courseid = args !! 0 -- courseid is the first and only argument ex. (!files -3085)
        title = pack ("Course Files (" ++ courseid ++ ")")
        icon = pack "https://www.clipartmax.com/png/middle/121-1218348_big-image-yellow-folder-icon-png.png"
    msgdata <- getFileStructure canvas_token courseid
    handleMessage m msgdata title icon


canvas_token :: String
canvas_token = "14589~soDe3Fvwq2zzG4ab8zqPOS7CcJIKsPSybnHE0sPjF7vFTEdGn2eoKaHN9VTUrYqy"

canvAssignments :: Message -> DiscordHandler ()
canvAssignments m = do
        let
                args = tail $ Split.splitOn "-" (removeSpace $ unpack $ messageText m)
                courseid = args !! 0 -- courseid is the first and only argument ex. (!assignments -3085)
                title = pack "Assignments"
                icon = pack "https://static.thenounproject.com/png/51139-200.png"
        msgdata <- getAssignments canvas_token courseid
        handleMessage m msgdata title icon

canvCourses :: Message -> DiscordHandler ()
canvCourses m = do
        let title = pack "Courses"
            icon = pack "https://cdn2.iconfinder.com/data/icons/online-university/96/computer_training_art_course-512.png"
        msgdata <- getCourses canvas_token
        handleMessage m msgdata title icon

{-
------------------------
END OF CANVAS DISCORD MESSAGE FUNCTIONS
------------------------
-}

{-
------------------------
START OF FILESTRUCTURE CODE
------------------------
-}
getFolders :: String -> String -> DiscordHandler (Maybe [Folder])
getFolders token courseid = do
    json <- apiRequest ("https://uppsala.instructure.com/api/v1/courses/" ++ courseid ++ "/folders") token
    let folders = Data.Aeson.decode $ BSL.fromStrict json
    return folders

getFiles :: String -> String -> DiscordHandler (Maybe [File])
getFiles token courseid = do
    json <- apiRequest ("https://uppsala.instructure.com/api/v1/courses/" ++ courseid ++ "/files") token
    let files = Data.Aeson.decode $ BSL.fromStrict json
    return files

--builds file structure of folderlist and filelist
fileStructure :: [Folder] -> [File] -> Folder
fileStructure folders files = sortFolders updatedfs
    where
        updatedfs = addFiles folders files

--sorts folderstructure in to 1 root folder
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
                    | Utils.fromMaybe (parent_id x) == r_id = buildFolder xs (Folder r_id r_name r_pid ((buildFolder (delete f folders) x):children) files) --if f has parent_id equal to root iq then add (f with its children) to children 
                    | otherwise = buildFolder xs root

--adds files to list of folders according to id
addFiles :: [Folder] -> [File] -> [Folder]
addFiles folders files = foldl searchFolder folders files
    where
        --calls aux function that keeps track of updated folder list
        searchFolder fs file = searchFolder' fs [] file
            where
                --searches for the folder the file belongs in and adds it to it, all folders are moved to updatedfs
                searchFolder' [] updatedfs _ = updatedfs
                searchFolder' ((Folder id name p_id c folder_files):xs) updatedfs file
                    | id == file_folder_id file = searchFolder' xs (Folder id name p_id c (file:folder_files):updatedfs) file
                    | otherwise = searchFolder' xs (Folder id name p_id c folder_files:updatedfs) file
            

hyperref :: [Char] -> [Char] -> [Char]
hyperref text url = "[" ++ text ++ "]" ++ "(" ++ url ++")"

--Converts root folder to tree
folderToTree :: Folder -> Tree String
folderToTree (Folder id name pid folders files) = Node name ( filesToTree files ++ foldersToTrees folders )

--a list of folders to a concated list of tree strings
foldersToTrees :: [Folder] -> [Tree String]
foldersToTrees = foldl (\treelst f -> treelst ++ [folderToTree f]) [] 

--list of files to list of tree strings
filesToTree :: [File] -> [Tree String]
filesToTree = map fileToTree

--file to tree string
fileToTree :: File -> Tree String
fileToTree (File _ name url _) = Node (hyperref name url) []