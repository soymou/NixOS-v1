-- lua/config/gp.lua
require("gp").setup {
  openai_api_key = os.getenv("OPENAI_API_KEY"), -- Load from environment

  agents = {
    {
      name = "ChatGPT-4",
      model = "gpt-4",
      system_prompt = "You are a helpful assistant who writes clean and idiomatic code and modularized configurations.",
    },
  },

  -- Optional: fallback agent if you don’t want to always use gpt-4
  default_agent = "ChatGPT-4",
}

