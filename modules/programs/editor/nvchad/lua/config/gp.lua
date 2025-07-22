-- lua/config/gp.lua
require("gp").setup {
  openai_api_key = os.getenv("OPENAI_API_KEY"),

  agents = {
    {
      name = "LLaMA 3",
      model = "llama3",
      provider = "ollama",
      system_prompt = "You're a helpful coding assistant.",
      temperature = 0.5,
    },
  },

  default_agent = "ChatGPT-4o",
}

