{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Calendar ( generateWeekPlan, realizePlan )
import Web.Scotty ( json, jsonData, post, scotty, liftIO, ActionM, middleware )
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Req
import Data.Aeson
import Data.Aeson.Types (parseMaybe, Parser)
import qualified Data.Text as T
import System.Environment (getEnv)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Company (keywords, personas, company)
import GHC.Generics (Generic)
import Data.Time (getCurrentTime)
import Domain (FinalCalendar, Comment)
import Api (ChatRequest(..), Message (..), Role (System, User), ChatResponse (ChatResponse, choices), ChatChoice (message))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.Wai.Middleware.Cors
import Network.HTTP.Types (methodGet, methodPost, methodPut, methodDelete)

data GenerateRequest = GenerateRequest
    { reqWeekIndex  :: Int
    , reqSubreddits :: [String]
    , reqKeywords   :: [String]
    , reqPostsPerWeek :: Int
    } deriving (Show, Generic)

instance FromJSON GenerateRequest

callAI :: String -> IO String
callAI prompt = runReq defaultHttpConfig $ do
    -- A. Construct the Typed Payload
    let reqPayload = ChatRequest
            { model       = "grok-4-1-fast-non-reasoning" 
            , messages    = 
                [ Message { role = System, content = "You are a helpful assistant." }
                , Message { role = User,   content = T.pack prompt } 
                ]
            , temperature = 0.7
            , stream      = False
            }

    apiKey <- liftIO $ getEnv "API_KEY"

    let url = https "api.x.ai" /: "v1" /: "chat" /: "completions"

    r <- req POST url (ReqBodyJson reqPayload) jsonResponse $
         oAuth2Bearer (B.pack apiKey)

    let respObj = responseBody r :: ChatResponse

    case choices respObj of
        (firstChoice : _) -> return $ T.unpack $ content $ message firstChoice
        []                -> liftIO $ fail "API returned empty choices list"

corsPolicy = cors (const $ Just policy)
    where
        policy = simpleCorsResourcePolicy
            { corsOrigins = Just (["https://acroatic.com", "http://localhost:5173", "http://localhost:4173"], True)
            , corsMethods = [methodGet, methodPost, methodPut, methodDelete, "OPTIONS"]
            , corsRequestHeaders = ["Authorization", "Content-Type"] 
            , corsMaxAge = Just 3600 
            }
            
main :: IO ()
main = scotty 3001 $ do
  middleware corsPolicy
  post "/generate-week" $ do
    req <- jsonData :: ActionM GenerateRequest
    now <- liftIO getCurrentTime

    let purePlan =
          generateWeekPlan
            now
            (reqWeekIndex req)
            (reqPostsPerWeek req)
            personas 
            (reqKeywords req) -- From Frontend
            (reqSubreddits req) -- From Frontend
    finalCalendar <- liftIO $ realizePlan callAI company purePlan

    json finalCalendar
