module Test where
import Domain (ContentPlan, ThreadBlueprint, FinalCalendar (..), Post (..), Persona (pName))
import Data.List (sortBy, sort, group)
import Data.Ord (comparing)

evaluateCalendar :: (String -> IO String) -> FinalCalendar -> IO FinalCalendar
evaluateCalendar aiFunc calendar = do
    let posts = fcPosts calendar

    -- Rule Checks
    let ruleWarnings = checkRules posts

    -- LLM Judge
    let sampleText = unlines $ map (\p -> "Post by " ++ pName (pAuthor p) ++ ": " ++ take 50 (pContent p)) (take 3 posts)
    let judgePrompt = "Role: QA Editor.\n" ++
                      "Task: Rate content variety (1-10). Return ONLY integer.\n" ++
                      "Content:\n" ++ sampleText

    scoreStr <- aiFunc judgePrompt
    let score = if length scoreStr < 3 then read scoreStr :: Int else 8

    return $ calendar
        { fcQualityScore = Just score
        , fcIssues = ruleWarnings ++ ["LLM Judge: " ++ scoreStr]
        }

checkRules :: [Post] -> [String]
checkRules posts =
    let
        -- Rule 1: Overposting
        subs = map pSubreddit posts
        groupedSubs = group (sort subs)
        subCounts = [ (s, length g) | g@(s:_) <- groupedSubs ]
                      
        overpostWarnings = [ "Warning: Overposting in " ++ s ++ " (" ++ show c ++ " times)" | (s,c) <- subCounts, c > 2 ]

        -- Rule 2: Sequential Repetition
        sortedPosts = sortBy (comparing pTimestamp) posts
        authors = map (pName . pAuthor) sortedPosts
        repetition = or $ zipWith (==) authors (drop 1 authors)
        repWarnings = (["Warning: Same persona posts consecutively" | repetition])

    in overpostWarnings ++ repWarnings
