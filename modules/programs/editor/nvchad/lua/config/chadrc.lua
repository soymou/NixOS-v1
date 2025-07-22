-- lua/config/chadrc.lua
---@type ChadrcConfig
local M = {}

M.ui = {
  theme = "nord",           -- Use NvChad's built-in Nord theme
  -- Optional: add toggle pair
  theme_toggle = { "nord", "onedark" }, 

  changed_themes = {
    nord = function()
      vim.cmd([[colorscheme nord]])
    end,
    onedark = function()
      vim.cmd([[colorscheme onedark]])
    end,
  },
}

return M

