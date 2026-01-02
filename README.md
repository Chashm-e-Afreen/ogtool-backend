# OGTool

OGTool (Organic Growth Tool) is a Haskell-based application designed to automate and simulate organic growth strategies on Reddit. It generates weekly content plans, including posts and engagement comments, using AI-powered personas.

This tool runs as a local web server that accepts requests to generate content calendars based on specific keywords, subreddits, and company goals. It leverages external LLM APIs (specifically configured for x.ai's Grok) to create authentic-sounding content.

## Features

*   **Weekly Content Planning**: Automatically generates a schedule of posts for a given week.
*   **Persona Management**: Uses distinct personas (e.g., "SaaS Founder", "Student", "Consultant") to vary the tone and perspective of content.
*   **Contextual Engagement**: Generates not just posts, but also realistic comment threads to simulate engagement.
*   **Smart Rotation**: Rotates through target keywords and subreddits to ensure diverse coverage.
*   **REST API**: Exposes a simple HTTP endpoint to trigger generation.

## Prerequisites

*   **Haskell Stack**: Ensure you have [Stack](https://docs.haskellstack.org/en/stable/README/) installed.
*   **x.ai API Key**: You need a valid API key for Grok (x.ai) set as an environment variable.

## Setup & Installation

1.  **Clone the repository:**
    ```bash
    git clone <repo-url>
    cd OGTool
    ```

2.  **Build the project:**
    ```bash
    stack build
    ```

3.  **Set the API Key:**
    You must set the `API_KEY` environment variable before running the application.
    ```bash
    export API_KEY="your_xai_api_key_here"
    ```

4.  **Run the server:**
    ```bash
    stack run
    ```
    The server will start on port `3001`.

## Usage

### Generate a Week Plan

Send a `POST` request to `/generate-week` to generate a content plan.

**Endpoint:** `POST http://localhost:3001/generate-week`

**JSON Body:**
```json
{
  "reqWeekIndex": 1,
  "reqSubreddits": ["r/startups", "r/marketing"],
  "reqKeywords": ["growth hacking", "saas marketing"],
  "reqPostsPerWeek": 3
}
```

**Example using `curl`:**
```bash
curl -X POST http://localhost:3001/generate-week \
  -H "Content-Type: application/json" \
  -d '{
    "reqWeekIndex": 1,
    "reqSubreddits": ["r/startups", "r/marketing"],
    "reqKeywords": ["growth hacking", "saas marketing"],
    "reqPostsPerWeek": 3
  }'
```

### Response

The server returns a JSON object containing the `FinalCalendar`, which includes a list of posts and comments scheduled for that week.

## Configuration

*   **Company Info & Personas**: Edit `Company.hs` to modify the company details, available personas, and default keyword/subreddit lists.
*   **LLM Model**: The model is currently hardcoded to `grok-4-1-fast-non-reasoning` in `Main.hs`. You can change this in the `callAI` function.

## Project Structure

*   `Main.hs`: Entry point. Handles the web server, API calls, and wiring everything together.
*   `Domain.hs`: Data types defining the core domain (Post, Persona, Calendar, etc.).
*   `Company.hs`: Configuration for the specific company being promoted and the personas used.
*   `Calendar.hs`: Logic for scheduling posts and rotating through keywords/subreddits.
*   `Api.hs`: Data types for the external LLM API (Request/Response structures).

## License

[Add License Here]
