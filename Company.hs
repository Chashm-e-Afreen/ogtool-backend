{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
import Data.List (List)

data Company where
  Company :: {website :: String, description :: String, subreddits :: [String], numOfPosts :: Int} -> Company
  
data Persona = Persona { name :: String, bio :: String, quirks :: [String] }
data Keyword = Keyword { id :: String, phrase :: String }

data Post = Post { 
    id :: String, 
    subreddit :: Subreddit, 
    author :: Persona, 
    content :: String, 
    timestamp :: UTCTime 
}

data Comment = Comment { 
    targetId :: String, 
    author :: Persona, 
    content :: String, 
    delayMinutes :: Int 
}
