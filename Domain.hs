{-# LANGUAGE DeriveGeneric #-}
module Domain where

import Data.Time (UTCTime)
import Data.Char (GeneralCategory)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

data Persona = Persona { 
    pName :: String, 
    pInfo :: String
} deriving (Generic)

data CompanyInfo = CompanyInfo { cName :: String, cDesc :: String, cWebsite :: String, cSubreddits :: [String], cPostsPerWeek :: Int } deriving (Generic)
data ContentPlan = ContentPlan {
    cpWeek :: Int,
    cpThreads :: [ThreadBlueprint]
} deriving (Generic)

data ThreadBlueprint = ThreadBlueprint {
    tbKeyword :: String,
    tbSubreddit :: String,
    tbPoster :: Persona,
    tbRepliers :: [Persona],
    tbPostTime :: UTCTime,
    tbPostIndex :: Int
} deriving (Generic)

data Post = Post { 
    postId :: String, 
    pSubreddit :: String, 
    pAuthor :: Persona, 
    pContent :: String, 
    pTimestamp :: UTCTime 
} deriving (Generic)

data Comment = Comment { 
    cTargetId :: String, 
    cAuthor :: Persona, 
    cContent :: String, 
    cDelayMinutes :: Int 
} deriving (Generic)

data FinalCalendar = FinalCalendar {
  fcWeekId       :: String
, fcPosts        :: [Post]
, fcComments     :: [Comment]
, fcQualityScore :: Maybe Int      
, fcIssues       :: [String]       
} deriving (Generic)

data RealizedThread = RealizedThread {
    rtPost :: Post,
    rtComments :: [Comment]
}

instance ToJSON Persona
instance ToJSON Comment
instance ToJSON Post
instance ToJSON FinalCalendar
