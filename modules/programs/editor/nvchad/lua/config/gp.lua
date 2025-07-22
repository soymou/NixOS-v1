
-- Clear any default configuration first
package.loaded["gp"] = nil

-- Correctly define only the Ollama configuration
require("gp").setup({
  providers = {
    ollama = {
      endpoint = "http://localhost:11434/v1/chat/completions", -- ✅ Corrected endpoint
      secret = "", -- no secret needed for Ollama
    },
  },

  agents = {
    {
      name = "llama3",
      chat = true,
      command = true,
      model = "llama3:latest", -- ✅ must match `ollama list`
      provider = "ollama",
      system_prompt = "You are a helpful coding assistant.",
      temperature = 0.5,
    },
  },

  default_agent = "llama3",
  default_command_agent = "llama3",
  default_chat_agent = "llama3",

  auto_detect_agents = false, -- ✅ prevent default agents

  -- Optional context (for working with file/project context)
  context = {
    headers = true,
    current_file = true,
    current_dir = true,
    project_files = false,
    max_tokens = 1000,
  },

  -- Optional UI tuning
  style_chat_finder_border = "rounded",
  style_popup_border = "rounded",

  log_level = "debug",

  -- Disable unused providers
  openai_api_key = nil,
  azure_api_key = nil,
  anthropic_api_key = nil,
})

