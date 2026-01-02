module Domain where

import Data.Time (UTCTime)

data Persona = Persona { 
    pId :: String, 
    pName :: String, 
    pBio :: String       }

data Keyword = Keyword { kId :: String, kPhrase :: String }
data CompanyInfo = CompanyInfo { cName :: String, cDesc :: String }

data ContentPlan = ContentPlan {
    cpWeek :: Int,
    cpThreads :: [ThreadBlueprint]
}

data ThreadBlueprint = ThreadBlueprint {
    tbKeyword :: Keyword,
    tbSubreddit :: String,
    tbPoster :: Persona,
    tbRepliers :: [Persona],     tbPostTime :: UTCTime
}

data FinalCalendar = FinalCalendar {
    weekId :: String,
    posts :: [Post],
    comments :: [Comment]
}
