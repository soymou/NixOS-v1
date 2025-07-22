-- Clear any default configuration first
package.loaded["gp"] = nil

-- Explicitly define only our Ollama configuration
require("gp").setup({
  -- Providers configuration - ONLY Ollama
  providers = {
    ollama = {
      endpoint = "http://localhost:11434/v1/chat/completions",
      secret = "",
    },
  },

  -- Agents configuration - ONLY our custom agents
  agents = {
    {
      name = "llama3",
      chat = true,
      command = true,
      model = "llama3:latest",
      provider = "ollama",
      system_prompt = "You are a helpful coding assistant.",
      temperature = 0.5,
    },
  },

  -- Default agents - explicitly set
  default_agent = "llama3",
  default_command_agent = "llama3", 
  default_chat_agent = "llama3",
  
  -- Disable agent auto-detection completely
  auto_detect_agents = false,
  
  -- Force cleanup of state on plugin load
  hooks = {
    InspectPlugin = function(plugin, params)
      -- Force our agents to be the only ones
      plugin.agents = {
        {
          name = "llama3",
          chat = true,
          command = true,
          model = "llama3:latest", 
          provider = "ollama",
          system_prompt = "You are a helpful coding assistant.",
          temperature = 0.5,
        },
      }
      -- Force our defaults
      plugin.config.default_agent = "llama3"
      plugin.config.default_command_agent = "llama3"
      plugin.config.default_chat_agent = "llama3"
    end,
  },
  
  -- Logging for debugging
  log_level = "debug",
  
  -- Disable all external API sources
  openai_api_key = nil,
  azure_api_key = nil,
  anthropic_api_key = nil,
  
  -- UI configuration
  style_chat_finder_border = "rounded",
  style_popup_border = "rounded",
})
