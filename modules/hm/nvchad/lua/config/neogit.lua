-- lua/config/neogit.lua
local neogit = require("neogit")

neogit.setup({})

vim.keymap.set("n", "<leader>ng", function()
  neogit.open()
end, { desc = "Open Neogit", noremap = true, silent = true })
