{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Api where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)

-- 1. Enforce valid Roles (No typos like "usre")
data Role = System | User | Assistant
    deriving (Show, Eq, Generic)

instance ToJSON Role where
    toJSON System    = "system"
    toJSON User      = "user"
    toJSON Assistant = "assistant"

-- 2. A single Message
data Message = Message
    { role    :: Role
    , content :: Text
    } deriving (Show, Generic)

instance ToJSON Message

-- 3. The Top-Level Request
data ChatRequest = ChatRequest
    { model       :: Text
    , messages    :: [Message]
    , temperature :: Double
    , stream :: Bool
    } deriving (Show, Generic)

instance ToJSON ChatRequest

data ChatChoice where
  ChatChoice :: {message :: Message} -> ChatChoice
  deriving (Show, Generic)

instance FromJSON Role where
    parseJSON = withText "Role" $ \t -> case t of
        "system"    -> return System
        "user"      -> return User
        "assistant" -> return Assistant
        _           -> fail "Invalid role"
        
instance FromJSON Message
instance FromJSON ChatChoice

data ChatResponse where
  ChatResponse :: {choices :: [ChatChoice]} -> ChatResponse
  deriving (Show, Generic)

instance FromJSON ChatResponse
