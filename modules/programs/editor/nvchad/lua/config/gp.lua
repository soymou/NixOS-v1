print("== Starting gp.lua config ==")

require("gp").setup {
  openai_api_key = false,
  openai_api_endpoint = "http://localhost:11434/v1",
  agents = {
    {
      name = "llama3",
      model = "llama3:latest",
      provider = "ollama",
      provider_opts = {
        api_base = "http://localhost:11434/v1",
      },
      system_prompt = "You're a helpful coding assistant.",
      temperature = 0.5,
    },
  },
  default_agent = "llama3",
  log_level = "debug",
}

print("== Finished gp.lua config ==")

