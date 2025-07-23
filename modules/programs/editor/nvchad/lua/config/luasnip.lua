local luasnip = require("luasnip")

luasnip.config.set_config({
  history = true,
  updateevents = "TextChanged,TextChangedI",
  enable_autosnippets = true,
})

require("luasnip.loaders.from_lua").lazy_load({
  paths = { vim.fn.stdpath("config") .. "/snippets" },
})
require("luasnip.loaders.from_vscode").lazy_load()

vim.cmd([[
  augroup luasnip_autoexpand
    autocmd!
    autocmd TextChangedI * lua require("luasnip").expand_auto()
  augroup END
]])

