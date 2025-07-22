-- lua/config/gp.lua
require("gp").setup {
  openai_api_key = os.getenv("OPENAI_API_KEY"),
  agents = {
    {
      name = "ChatGPT-4",
      model = "gpt-4",
    },
  },
}
