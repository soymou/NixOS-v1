local lspconfig = require("lspconfig")

-- Lua
lspconfig.lua_ls.setup({})

-- Nix
lspconfig.nixd.setup({})

-- Bash
lspconfig.bashls.setup({})

-- Docker
lspconfig.dockerls.setup({})
lspconfig.docker_compose_language_service.setup({})

-- Emmet
lspconfig.emmet_language_server.setup({})

-- Python
lspconfig.pylsp.setup({
  settings = {
    pylsp = {
      plugins = {
        flake8 = { enabled = true },
      },
    },
  },
})

-- LaTeX
lspconfig.texlab.setup({
  settings = {
    texlab = {
      build = {
        executable = "latexmk",
        args = { "-pdf", "-interaction=nonstopmode", "-synctex=1", "%f" },
        onSave = true,
      },
      forwardSearch = {
        executable = "zathura", -- or another PDF viewer
        args = { "--synctex-forward", "%l:1:%f", "%p" },
      },
    },
  },
})

