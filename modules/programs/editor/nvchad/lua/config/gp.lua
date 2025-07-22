-- lua/config/gp.lua
require("gp").setup {
  openai_api_key = os.getenv("OPENAI_API_KEY"),

  agents = {
    {
      name = "ChatGPT-4o",
      model = "gpt-4o", 
      system_prompt = "You are a helpful coding assistant who writes clear, idiomatic code.",
    },
  },

  default_agent = "ChatGPT-4o",
}

