{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Company where
import Domain (Persona (..), CompanyInfo(..))

personas :: [Persona]
personas =
  [ Persona
      { pName = "Riley Hart",
        pInfo = "Head of Operations at a SaaS startup. A perfectionist who became the unofficial owner of every internal deck. He uses Slideforge because misaligned headers drive him crazy, and it allows him to focus on the narrative spine of the company rather than formatting adjustments."
      },
    Persona
      { pName = "Jordan Brooks",
        pInfo = "Independent Consultant for early-stage founders. A natural storyteller who needs slides that match the high quality of his strategic thinking. He uses Slideforge to instantly create clean flows that allow his story to land with clients without hitting a 'design wall'."
      },
    Persona
      { pName = "Emily Chen",
        pInfo = "Economics student and the 'unofficial slide maker' for every group project. She is overcommitted and tired of tweaking fonts at 1am. She uses Slideforge to turn outlines into professional slides instantly so she can actually sleep."
      },
    Persona
      { pName = "Alex Ramirez",
        pInfo = "Head of Sales at a mid-market SaaS company. Believes in showing up sharp and winning enterprise deals. He uses Slideforge to ensure his entire team's pitch decks look consistent and polished, rather than having reps create messy, unbranded slides."
      },
    Persona
      { pName = "Priya Nandakumar",
        pInfo = "Product Manager who translates between engineering, design, and leadership. She uses Slideforge to visualize complex roadmaps and dependencies clearly, making her stories feel honest and digestible without draining her energy on design."
      }
  ]

company :: CompanyInfo
company =
  CompanyInfo
    { cName = "Slideforge.ai",
      cWebsite = "https://slideforge.ai",
      cDesc = "Slideforge is an AI-powered presentation and storytelling tool that turns outlines or rough notes into polished, professional slide decks. It supports exporting to PowerPoint, Google Slides, and PDF, offers branded templates, and provides an API for developers to integrate slide generation.",
      cSubreddits =
        [ "r/PowerPoint",
          "r/GoogleSlides",
          "r/consulting",
          "r/marketing",
          "r/entrepreneur",
          "r/startups",
          "r/smallbusiness",
          "r/business",
          "r/productivity",
          "r/AskAcademia",
          "r/teachers",
          "r/education",
          "r/Canva",
          "r/ChatGPT",
          "r/ChatGPTPro",
          "r/ClaudeAI",
          "r/artificial",
          "r/design",
          "r/contentcreation",
          "r/presentations"
        ],
      cPostsPerWeek = 3
    }

keywords :: [String]
keywords = 
    [ "best ai presentation maker"
    , "ai slide deck tool"
    , "pitch deck generator"
    , "alternatives to PowerPoint"
    , "how to make slides faster"
    , "design help for slides"
    , "Canva alternative for presentations"
    , "Claude vs Slideforge"
    , "best tool for business decks"
    , "automate my presentations"
    , "need help with pitch deck"
    , "tools for consultants"
    , "tools for startups"
    , "best ai design tool"
    , "Google Slides alternative"
    , "best storytelling tool"
    ]

