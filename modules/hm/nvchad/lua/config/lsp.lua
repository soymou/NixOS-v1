vim.defer_fn(function()
  local lspconfig = require("lspconfig")

  lspconfig.lua_ls.setup({})

  lspconfig.nixd.setup({})
  lspconfig.bashls.setup({})

  lspconfig.dockerls.setup({})
  lspconfig.docker_compose_language_service.setup({})

  lspconfig.emmet_language_server.setup({})

  lspconfig.pylsp.setup({
    settings = {
      pylsp = {
        plugins = {
          flake8 = { enabled = true },
        },
      },
    },
  })

  lspconfig.texlab.setup({
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
      },
    },
  })
end, 100)

