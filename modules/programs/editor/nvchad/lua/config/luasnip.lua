local ls = require("luasnip")

ls.config.set_config({
  history = true,
  updateevents = "TextChanged,TextChangedI",
  enable_autosnippets = true,
})

-- Optional: map Tab to expand or jump in insert mode
vim.api.nvim_set_keymap("i", "<Tab>", [[luasnip.expand_or_jumpable() and "<Plug>luasnip-expand-or-jump" or "<Tab>"]], { expr = true, silent = true })
vim.api.nvim_set_keymap("s", "<Tab>", [[luasnip.jumpable(1) and "<Plug>luasnip-jump-next" or "<Tab>"]], { expr = true, silent = true })

require("luasnip.loaders.from_lua").lazy_load({ paths = { "~/.config/nvim/snippets" } })
