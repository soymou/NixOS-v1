local ls = require("luasnip")
local cmp = require("cmp")

ls.config.set_config({
  history = true,
  updateevents = "TextChanged,TextChangedI",
  enable_autosnippets = true,
})

-- Tab completion function for insert/select modes
function _G.tab_complete(fallback)
  local col = vim.fn.col('.') - 1
  local line = vim.fn.getline('.')

  if cmp.visible() then
    cmp.select_next_item()
  elseif ls.expand_or_jumpable() then
    ls.expand_or_jump()
  elseif col == 0 or line:sub(col, col):match("%s") then
    fallback()  -- Insert a real tab character
  else
    cmp.complete()
  end
end

function _G.shift_tab_complete(fallback)
  if cmp.visible() then
    cmp.select_prev_item()
  elseif ls.jumpable(-1) then
    ls.jump(-1)
  else
    fallback()
  end
end

-- Map Tab and Shift-Tab keys in insert and select modes to our functions
vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", { expr = true, silent = true })
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", { expr = true, silent = true })
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.shift_tab_complete()", { expr = true, silent = true })
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.shift_tab_complete()", { expr = true, silent = true })

require("luasnip.loaders.from_lua").lazy_load({ paths = { "~/.config/nvim/snippets" } })
