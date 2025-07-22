require("gp").setup {
  providers = {
    ollama = {
      endpoint = "http://localhost:11434/v1",
      secret = "", -- no secret needed for local Ollama usually
    },
  },

  agents = {
    {
      name = "llama3",
      model = "llama3",
      provider = "ollama",
      system_prompt = "You're a helpful coding assistant.",
      temperature = 0.5,
    },
  },

  default_agent = "llama3",
  log_level = "debug",
  auto_detect_agents = false,  -- disable auto detection to prevent showing other agents
}
