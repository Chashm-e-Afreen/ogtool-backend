module Calendar where

generateWeekPlan :: Int -> [Persona] -> [Keyword] -> [String] -> ContentPlan
generateWeekPlan weekNum allPersonas allKeywords allSubreddits =
    let
        postsPerWeek = 3
        
        -- A. Keyword Selection (Infinite Rotation)
        -- Uses 'drop' and 'take' to cycle through the list forever
        startIndex = (weekNum * postsPerWeek) `mod` (length allKeywords)
        selectedKeywords = take postsPerWeek $ drop startIndex (cycle allKeywords)

        -- B. Subreddit Selection (Round Robin)
        -- Ensures we don't spam the same sub every post
        subStartIndex = (weekNum * postsPerWeek) `mod` (length allSubreddits)
        selectedSubs = take postsPerWeek $ drop subStartIndex (cycle allSubreddits)

        -- C. Persona Pairing (The "No Self-Reply" Logic)
        -- We zip the keywords with a rotated list of personas
        threadBlueprints = zipWith3 (createBlueprint weekNum allPersonas) 
                                    [0..] 
                                    selectedKeywords 
                                    selectedSubs
    in
        ContentPlan weekNum threadBlueprints

-- Helper: Create a single thread scenario
createBlueprint :: Int -> [Persona] -> Int -> Keyword -> String -> ThreadBlueprint
createBlueprint weekNum personas postIndex keyword subreddit =
    let
        -- Deterministic Shuffle based on Week + PostIndex
        -- (Ensures Riley isn't always the poster)
        rotatedPersonas = rotateList (weekNum + postIndex) personas
        
        poster = head rotatedPersonas
        
        -- Take the next 2 distinct personas as repliers
        repliers = take 2 (drop 1 rotatedPersonas)
        
        -- Calculate Time (e.g., Monday + postIndex * 2 days)
        baseTime = calculateBaseTime weekNum postIndex 
    in
        ThreadBlueprint keyword subreddit poster repliers baseTime

-- Simple list rotator
rotateList :: Int -> [a] -> [a]
rotateList n xs = take (length xs) . drop (n `mod` length xs) . cycle $ xs
