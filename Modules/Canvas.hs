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
import Data.Maybe

{-
------------------------
START OF TYPES
------------------------
-}

{- Folder
  Represents a folder. 

  folder_id - id of the folder in canvas
  folder_name - name of the folder
  parent_id - id of the parent folder
  children - list of child folders
  files - list of child files
-}

data Folder = Folder {
    folder_id :: String,
    folder_name :: String, 
    parent_id :: Maybe String, --root folder has no parent_id
    children :: [Folder],
    files :: [File]
} deriving (Show)

{- File
  Represents a file. 

  file_id - id of the file in canvas
  file_name - name of the file
  file_url - url of the file
  file_folder_id - id of the parent folder
-}
data File = File {
    file_id :: String,
    file_name :: String,
    file_url :: String,
    file_folder_id :: String
} deriving (Show,Eq)

{- Course
  Represents a course in canvas.

  course_id - id of the course in canvas
  course_name - name of the course
  course_code - code of the course
-}
data Course = Course { 
  course_id   :: String
, course_name :: String
, course_code :: String
} deriving (Data,Typeable,Show)

{- Assignment
  Represents an assignment in canvas.

  assignment_id - id of the assignment in canvas
  assignment_name - name of the assignment
  points_possible - assignment max points
  assignment_url - url of the assignment
  created_at - creation date
-}
data Assignment = Assignment {
    assignment_id :: String,
    assignment_name :: String,
    points_possible :: String,
    assignment_url :: String,
    created_at :: String
} deriving (Data,Typeable,Show)

{-
------------------------
START OF INSTANCES
------------------------
-}

-- defines Eq (==) for the folder type
instance Eq Folder where
    (==) f1 f2 = folder_id f1 == folder_id f2 && 
                                folder_name f1 == folder_name f2 && 
                                parent_id f1 == parent_id f2 && 
                                children f1 == children f2 && 
                                files f1 == files f2

-- defines JSON parsing for the folder type
instance FromJSON Folder where
    parseJSON (Object v) = do
        id <- v .: "id" 
        name <- v .: "name"
        parent <- v .:? "parent_folder_id" -- .:? takes into account for Null and nonexisting keys
        return $ Folder (show (id ::Int)) name (maybeToString (parent :: Maybe Int)) [] [] --new folder does not have children nor files
    parseJSON _ = mempty

-- defines JSON parsing for the file type
instance FromJSON File where
    parseJSON (Object v) = do
        id <- v .: "id"
        name <- v .: "display_name"
        url <- v .: "url"
        folderid <- v .: "folder_id"
        return $ File (show (id :: Int)) name url (show (folderid :: Int))
    parseJSON _ = mempty

-- defines JSON parsing for the course type
instance FromJSON Course where
    parseJSON (Object v) = do
        id <- v .: "id" 
        name <- v .: "name"
        code <- v .: "course_code"
        return (Course {course_id = (show (id :: Int)),course_name= name,course_code= code})
    parseJSON _ = mempty

-- defines JSON parsing for the assignment type
instance FromJSON Assignment where
    parseJSON (Object v) = do
        id <- v .: "id" 
        name <- v .: "name"
        points <- v .: "points_possible"
        url <- v .: "html_url"
        created_at <- v .: "created_at"
        return $ Assignment (show (id :: Int)) name (show (points :: Double)) url created_at
    parseJSON _ = mempty

-- defines stringIt for the Course type
instance Stringable Course where
    stringIt (Course id name code) = name ++ "\nID: " ++ id ++ "\nCODE: " ++ code

-- defines stringIt for the Assignment type
instance Stringable Assignment where
    stringIt (Assignment id name points url creation) = name ++ "\nID: " ++ id ++ "\nPOINTS: " ++ points ++ "\nURL: " ++ url ++ "\nCREATION DATE: " ++ creation

-- defines stringIt for the Folder type
instance Stringable Folder where
    stringIt folder = TreePrint.treeDraw $ folderToTree $ folder

{-
------------------------
START OF GET DATA FUNCTIONS
------------------------
-}
----------------------------------------------------------------------
{- getCourses token
     Gets a list of courses for a canvas user
     PRE: --
     RETURNS: messagedata containing a list of courses
     SIDE EFFECTS: performs a http request
     EXAMPELS --
  -}

getCourses :: String -> DiscordHandler (MessageData (Maybe [Course]))
getCourses token = do
    json <- apiRequest "https://uppsala.instructure.com/api/v1/users/self/courses" token
    let courses = Data.Aeson.decode $ BSL.fromStrict json
    toMessageData courses

{- getAssignments token courseid
     Gets a list of assignments from a course with the corresponding courseid
     PRE: --
     RETURNS: messagedata containing a list of assignments
     SIDE EFFECTS: performs a http request
     EXAMPELS --
  -}

getAssignments :: String -> String -> DiscordHandler (MessageData (Maybe [Assignment]))
getAssignments token courseid = do
    json <- apiRequest ("https://uppsala.instructure.com/api/v1/courses/" ++ courseid ++ "/assignments") token
    let assignments = Data.Aeson.decode $ BSL.fromStrict json -- json to Maybe [Assignment]
    toMessageData assignments

{- getFileStructure token courseid
     Gets the root folder for folders/files in a canvas course with the corresponding courseid
     PRE: --
     RETURNS: messagedata containing a list of assignments
     SIDE EFFECTS: performs http requests
     EXAMPELS --
  -}

getFileStructure :: String -> String -> DiscordHandler (MessageData (Maybe Folder))
getFileStructure token courseid = do
    folders <- getFolders token courseid
    files <- getFiles token courseid
    if isNothing folders || isNothing files --if api failed in any of the requests
    then do toMessageData Nothing
    else do 
     let folder = fileStructure (Utils.fromMaybe folders) (Utils.fromMaybe files) -- builds root folder
     toMessageData (Just folder)


{-
---------------------------------------------------
START OF CANVAS DISCORD MESSAGE FUNCTIONS
---------------------------------------------------
-}
canvas_token :: String
canvas_token = "14589~soDe3Fvwq2zzG4ab8zqPOS7CcJIKsPSybnHE0sPjF7vFTEdGn2eoKaHN9VTUrYqy"

{- canvCourses message
     Sends a discord embed containing the courses for a canvas user
     PRE: --
     RETURNS: --
     SIDE EFFECTS: perfoms a http request and performs a rest call to the discord api
     EXAMPLES: -- 
  -}
canvCourses :: Message -> DiscordHandler ()
canvCourses m = do
        let title = pack "Courses"
            icon = pack "https://cdn2.iconfinder.com/data/icons/online-university/96/computer_training_art_course-512.png"
        msgdata <- getCourses canvas_token
        handleMessage m msgdata title icon

{- canvAssignments message
     Sends a discord embed containing the assignments for a canvas course
     PRE: --
     RETURNS: --
     SIDE EFFECTS: perfoms a http request and performs a rest call to the discord api
     EXAMPLES: -- 
  -}
canvAssignments :: Message -> DiscordHandler ()
canvAssignments m = do
        let
                args = tail $ Split.splitOn "-" (removeSpace $ unpack $ messageText m)
                courseid = args !! 0 -- courseid is the first and only argument ex. (!assignments -3085)
                title = pack "Assignments"
                icon = pack "https://static.thenounproject.com/png/51139-200.png"
        msgdata <- getAssignments canvas_token courseid
        handleMessage m msgdata title icon

{- canvFiles message
     Sends a discord embed containing the filestructure for a canvas course
     PRE: --
     RETURNS: --
     SIDE EFFECTS: perfoms http requests and performs a rest call to the discord api
     EXAMPLES: -- 
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

{-
------------------------
START OF FILESTRUCTURE CODE
------------------------
-}
{- getFolders token courseid
     Gets the folders in a canvas course
     PRE:
     RETURNS: Nothing || List of folders in a canvas course
     SIDE EFFECTS: 
     EXAMPLES: --
  -}

getFolders :: String -> String -> DiscordHandler (Maybe [Folder])
getFolders token courseid = do
    json <- apiRequest ("https://uppsala.instructure.com/api/v1/courses/" ++ courseid ++ "/folders") token
    let folders = Data.Aeson.decode $ BSL.fromStrict json
    return folders

{- getFiles token courseid
     Gets the files in a canvas course
     PRE:
     RETURNS: Nothing || List of files in a canvas course
     SIDE EFFECTS: 
     EXAMPLES: --
  -}

getFiles :: String -> String -> DiscordHandler (Maybe [File])
getFiles token courseid = do
    json <- apiRequest ("https://uppsala.instructure.com/api/v1/courses/" ++ courseid ++ "/files") token
    let files = Data.Aeson.decode $ BSL.fromStrict json
    return files

{- fileStructure folders files
     Builds a complete file-/folderstructure in a root folder
     PRE: --
     RETURNS: root folder containing all child folders and files
     SIDE EFFECTS: 
     EXAMPLES: addFiles [(Folder "1" "root" Nothing [] []),(Folder "2" "root child" (Just "1") [])] [(File "101" "file1" "www.file.com" "1")]
                == [(Folder "1" "root" Nothing [(Folder "2" "root child" (Just "1") [])] [(File "101" "file1" "www.file.com" "1")])]
  -}
fileStructure :: [Folder] -> [File] -> Folder
fileStructure folders files = sortFolders updatedfldrs
    where
        updatedfldrs = addFiles folders files


{- sortFolders folders
     Sorts list of folders in to 1 root folder containing its child folders
     PRE: There should be 1 and only 1 root folder in the list
     RETURNS: root folder containing its child folders
     SIDE EFFECTS: --
     EXAMPLES: sortFolders [(Folder "1" "root" Nothing [] []),(Folder "2" "root child" (Just "1") [] [])]
                == Folder "1" "root" Nothing [Folder "2" "root child" (Just "1") [] []] []
  -}
sortFolders :: [Folder] -> Folder
sortFolders folderList = findRoot folderList folderList
    where
        {- findRoot folderList folders
                Finds the root folder and then calls buildFolder
                PRE: folderList = folders
                RETURNS: the result of buildFolder
        -}
        --VARIANT: folderList
        findRoot:: [Folder] -> [Folder] -> Folder
        findRoot (f:fs) folders = -- searches for root folder
            if parent_id f == Nothing then buildFolder (delete f folders) f -- when root folder is found, calls aux function and removes root from folders
            else findRoot fs folders
            where
                {- buildFolder folders root
                Builds folder structure from root folder to bottom
                PRE: folders does not contain root
                RETURNS: root folder containing its child folders 
                SIDE EFFECTS: --
                EXAMPLES: sortFolders [(Folder "2" "root child" (Just "1") [] [])] (Folder "1" "root" Nothing [] [])
                == Folder "1" "root" Nothing [Folder "2" "root child" (Just "1") [] []] []
        -}      --VARIANT folders
                buildFolder :: [Folder] -> Folder -> Folder
                buildFolder [] root = root
                buildFolder (x:xs) root@(Folder r_id r_name r_pid children files)
                    | Utils.fromMaybe (parent_id x) == r_id = buildFolder xs (Folder r_id r_name r_pid ((buildFolder (delete f folders) x):children) files) --if f has parent_id equal to root iq then add (f with its children) to children 
                    | otherwise = buildFolder xs root

{- addFiles folders files
     Adds files to their parent folders
     PRE: --
     RETURNS: updated folders containing their child files
     SIDE EFFECTS: --
     EXAMPLES: addFiles [(Folder "1" "root" Nothing [] []),(Folder "2" "root child" (Just "1") [])] [(File "101" "file1" "www.file.com" "1")]
                == [(Folder "1" "root" Nothing [] [(File "101" "file1" "www.file.com" "1")]),(Folder "2" "root child" (Just "1") [])]
  -}
addFiles :: [Folder] -> [File] -> [Folder]
addFiles folders files = foldl searchFolder folders files
    where
        {- searchFolder fs updatedfs file
                Adds file to its parent folder and rebuilds an updated version of folders
                PRE: --
                RETURNS: updated version of folders containing file
                SIDE EFFECTS: --
                EXAMPLES: 
        -}
        searchFolder :: [Folder] -> File -> [Folder]
        searchFolder fs file = searchFolder' fs [] file
            where
                {- searchFolder' fs updatedfs file
                searchFolder with accumulator as an extra argument because a function in a foldl only can take 2 arguments
               -}
                --VARIANT: length fs
                searchFolder' :: [Folder] -> [Folder] -> File -> [Folder]
                searchFolder' [] updatedfs _ = updatedfs
                searchFolder' ((Folder id name p_id c folder_files):xs) updatedfs file
                    | id == file_folder_id file = searchFolder' xs (Folder id name p_id c (file:folder_files):updatedfs) file
                    | otherwise = searchFolder' xs (Folder id name p_id c folder_files:updatedfs) file
            



{- folderToTree root
     Converts a root folder to a tree string node
     PRE: --
     RETURNS: the root as a tree string node containing its file/folder children in tree string format
     SIDE EFFECTS: --
     EXAMPLES: folderToTree (Folder "1" "root" Nothing [Folder "2" "root child" (Just "1") [] []] [])
                == Node "root" [Node "root child" []]
  -}
folderToTree :: Folder -> Tree String
folderToTree (Folder _ name _ folders files) = Node name ( filesToTree files ++ foldersToTrees folders )


{- foldersToTree files
     Converts a list of folders to a list of tree string nodes
     PRE: --
     RETURNS: a list of tree string nodes containing all folders and files from their corresponding folder
     SIDE EFFECTS: --
     EXAMPLES: foldersToTree [(Folder "1" "root" Nothing [] []),(Folder "2" "root child" (Just "1") [])]
                == Node "root" [Node "root child" []]
  -}
foldersToTrees :: [Folder] -> [Tree String]
foldersToTrees = foldl (\treelst f -> treelst ++ [folderToTree f]) [] 


{- fileToTree file@(File id name url folderid)
     Converts file into a tree string node
     PRE: --
     RETURNS: Node ("[name](url)") []
     SIDE EFFECTS: --
     EXAMPLES: fileToTree (File "508" "example.png" "www.example.org" "1") == Node "[example.png](www.example.org)" []
  -}

fileToTree :: File -> Tree String
fileToTree (File _ name url _) = Node (hyperref name url) []

{- filesToTree files
     Converts a list of files into a list of tree string nodes
     PRE: --
     RETURNS: a list of tree string nodes
     SIDE EFFECTS: --
     EXAMPLES: filesToTree [(File "508" "example.png" "www.example.org" "1"),(File "111" "test.png" "www.google.com" "2")] 
                == [Node "[example.png](www.example.org)" [], Node "[test.png](www.google.com)" []]
  -}
filesToTree :: [File] -> [Tree String]
filesToTree = map fileToTree