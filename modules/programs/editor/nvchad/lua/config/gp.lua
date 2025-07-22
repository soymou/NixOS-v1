-- lua/config/gp.lua
require("gp").setup {
  openai_api_key = os.getenv("OPENAI_API_KEY"),

  agents = {
    {
      name = "llama3",
      model = "llama3:latest",
      provider = "ollama",
      system_prompt = "You're a helpful coding assistant.",
      temperature = 0.5,
    },
  },

  default_agent = "llama3",
  log_level = "debug",
}

