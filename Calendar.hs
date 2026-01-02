module Calendar where

import Domain
import Data.Time (UTCTime(..), Day, addDays, secondsToDiffTime)
import Data.List (intercalate)
import Control.Concurrent.Async (mapConcurrently)


calculateBaseTime :: UTCTime -> Int -> Int -> UTCTime
calculateBaseTime startTime weekNum postIndex =
    let
        -- Calculate total days to add from the project start
        daysToAdd = (fromIntegral weekNum * 7) + (fromIntegral postIndex * 2)

        -- Add days to the date part of UTCTime
        newDay = addDays daysToAdd (utctDay startTime)

        -- Keep the same time of day (or randomize it here if you want)
        sameTime = utctDayTime startTime
    in
        UTCTime { utctDay = newDay, utctDayTime = sameTime }

createBlueprint :: UTCTime -> Int -> [Persona] -> Int -> String -> String -> ThreadBlueprint
createBlueprint startTime weekNum personas postIndex keyword subreddit =
    let
        -- Deterministic Rotation based on Week + PostIndex
        rotationCount = weekNum + postIndex
        rotatedPersonas = rotateList rotationCount personas

        -- Safe Pattern Matching to get Poster + Repliers
        (poster, repliers) = case rotatedPersonas of
            (p : r1 : r2 : _) -> (p, [r1, r2])
            (p : rest)        -> (p, take 2 (cycle rest))
            []                -> error "Persona list cannot be empty!"

        -- Calculate Time
        baseTime = calculateBaseTime startTime weekNum postIndex
    in
        -- THE FIX: We now include 'postIndex' as the first argument
        ThreadBlueprint
            { tbPostIndex = postIndex
            , tbKeyword = keyword
            , tbSubreddit = subreddit
            , tbPoster = poster
            , tbRepliers = repliers
            , tbPostTime = baseTime
            }

-- | Safe List Rotator
rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs =
    let len = length xs
        -- Use modulo to avoid unnecessary cycles
        effectiveN = n `mod` len
    in drop effectiveN xs ++ take effectiveN xs

-- Updated Main Logic
generateWeekPlan :: UTCTime -> Int -> Int -> [Persona] -> [String] -> [String] -> ContentPlan
generateWeekPlan startTime weekNum postsPerWeek allPersonas allKeywords allSubreddits =
    let
        posts = min postsPerWeek 7
        -- Keywords: standard cycle logic
        startIndex = (weekNum * postsPerWeek) `mod` length allKeywords
        selectedKeywords = take postsPerWeek $ drop startIndex (cycle allKeywords)

        -- Subreddits: standard cycle logic
        subStartIndex = (weekNum * postsPerWeek) `mod` length allSubreddits
        selectedSubs = take postsPerWeek $ drop subStartIndex (cycle allSubreddits)

        -- Zip safely
        threadBlueprints = zipWith3 (createBlueprint startTime weekNum allPersonas)
                                    [0..]
                                    selectedKeywords
                                    selectedSubs
    in
        ContentPlan weekNum threadBlueprints

constructPostPrompt :: Persona -> String -> String -> String
constructPostPrompt author keyword subreddit =
    unlines
        [ "Role: You are " ++ pName author
        , "Bio: " ++ pInfo author
        , "Task: Write a Reddit post for r/" ++ subreddit
        , "Topic: Ask a question about '" ++ keyword ++ "'"
        , "Constraint 1: Use a specific detail from your bio."
        , "Constraint 2: Do not sound like a marketer."
        , "Output: Just the post text, nothing else."
        ]

constructReplyPrompt :: Persona -> String -> String -> String
constructReplyPrompt author parentContent companyName =
    unlines
        [ "Role: You are " ++ pName author
        , "Bio: " ++ pInfo author
        , "Task: Reply to this Reddit comment: \"" ++ parentContent ++ "\""
        , "Goal: Agree with them and subtly recommend " ++ companyName
        , "Constraint: Keep it under 2 sentences."
        ]

-- A generic type for your LLM function
type LLMGenerator = String -> IO String

realizePlan :: LLMGenerator -> CompanyInfo -> ContentPlan -> IO FinalCalendar
realizePlan callAI company plan = do
    -- 1. Generate the Threads (Post + bundled Comments)
    realizedThreads <- mapConcurrently (generateThreadContent callAI company) (cpThreads plan)

    -- 2. Flatten the structure for the Final View
    let allPosts = map rtPost realizedThreads
    let allComments = concatMap rtComments realizedThreads

    -- 3. Return the corrected FinalCalendar type
    return $ FinalCalendar
        { fcWeekId = "WEEK-" ++ show (cpWeek plan)
        , fcPosts = allPosts
        , fcComments = allComments
        , fcQualityScore = Nothing -- Will be filled by Evaluate phase
        , fcIssues = []
        }
        
generateComment :: LLMGenerator -> CompanyInfo -> String -> String -> Persona -> IO Comment
generateComment callAI company parentId parentText replier = do

    -- 1. Build the specific prompt for a reply
    -- We include the parent text so the LLM knows what it is replying to.
    let prompt = unlines
            [ "Role: You are " ++ pName replier
            , "Bio: " ++ pInfo replier
            , "Task: Write a Reddit comment replying to this post: " ++ show parentText
            , "Objective: Validate the original poster's pain point."
            , "Strategy: varied. If PostIndex is even: Complain about the problem, share a war story, do NOT mention any products. If PostIndex is odd: Briefly mention" ++ cName company ++ "helped you, but be low-key. No marketing fluff, no emojis, no 'game-changer'."
            , "Constraint: Keep it under 280 characters. Do not sound like a bot."
            ]

    commentText <- callAI prompt

    return $ Comment
        { cTargetId = parentId
        , cAuthor = replier
        , cContent = commentText
        , cDelayMinutes = 5
        }

generateThreadContent :: LLMGenerator -> CompanyInfo -> ThreadBlueprint -> IO RealizedThread
generateThreadContent callAI company blueprint = do
    -- 1. Build the prompt using the Pure logic helper
    let postPrompt = constructPostPrompt
                        (tbPoster blueprint)
                        (tbKeyword blueprint)
                        (tbSubreddit blueprint)

    -- 2. EXECUTE IO: Call OpenAI to get the post text
    --    This is where 'postText' is created.
    postText <- callAI postPrompt

    -- 3. Construct the Post ID (e.g., "P1")
    --    We need this to link comments to this post.
    let postId = "P" ++ show (tbPostIndex blueprint)

    -- 4. Create the Post Record
    let post = Post {
        postId = postId,
        pSubreddit = tbSubreddit blueprint,
        pAuthor = tbPoster blueprint,
        pContent = postText,   -- <--- Used here to fill the post body
        pTimestamp = tbPostTime blueprint
    }

    -- 5. Generate Replies
    --    We pass 'postText' here so the repliers know what they are replying to.
    comments <- mapM (generateComment callAI company postId postText)
                     (tbRepliers blueprint)

    return $ RealizedThread
        { rtPost = post
        , rtComments = comments
        }
