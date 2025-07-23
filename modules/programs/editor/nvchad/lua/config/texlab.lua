
local lspconfig = require("lspconfig")
local util = require("lspconfig.util")

lspconfig.texlab.setup({
  root_dir = util.root_pattern("main.tex", ".git"), -- 👈 Add this
  on_attach = function(client, bufnr)
    -- Optional keybindings or settings here
  end,
  settings = {
    texlab = {
      build = {
        executable = "latexmk",
        args = { "-pdf", "-interaction=nonstopmode", "-synctex=1", "%f" },
        onSave = true,
      },
      forwardSearch = {
        executable = "zathura",
        args = { "--synctex-forward", "%l:1:%f", "%p" },
      },
      completion = {
        includePaths = { "." },
        snippetSupport = true,
      },
    },
  },
})

