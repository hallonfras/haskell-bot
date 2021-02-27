module DataT where

data Course = Course { 
  course_id   :: String
, course_name :: String
, course_code :: String
} deriving (Show)

data Assignment = Assignment {
    assignment_id :: String,
    assignment_name :: String,
    points_possible :: String,
    assignment_url :: String,
    created_at :: String
} deriving (Show)


