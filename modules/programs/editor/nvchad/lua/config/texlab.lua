local lspconfig = require("lspconfig")

lspconfig.texlab.setup({
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
        -- 👇 Add this block
        includePaths = { "." },
        -- Optional: enable snippet expansion for commands
        snippetSupport = true,
      },
    },
  },
})

