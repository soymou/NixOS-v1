''
  require('mdx').setup()

  vim.treesitter.language.register('tsx', 'mdx')

  require('nvim-treesitter.configs').setup {
    highlight = {
      enable = true,
      additional_vim_regex_highlighting = { "markdown", "mdx" },
    },
  }

  -- LuaSnip snippets
  local ls = require("luasnip")
  local s = ls.snippet
  local t = ls.text_node
  local i = ls.insert_node
  local fmt = require("luasnip.extras.fmt").fmt

  -- Ensure mdx filetype is recognized by LuaSnip
  ls.filetype_extend("mdx", {"markdown"})

  ls.add_snippets("mdx", {
    s("code", fmt([[
```code :language {} :eval {}
{}
```
]], {
      i(1, "python"),
      i(2, "false"),
      i(0)
    }))
  })

 ls.add_snippets("mdx", {
   s("ty", fmt([[
```typst
{}
```
]], {
     i(0)
   }))
 })

 ls.add_snippets("mdx", {
   s("aside", fmt([[
<div id="{}">
<Aside type = "{}" title = "{}">
{}
</Aside>
</div>
{}]], {
     i(1),
     i(2, "tip"),
     i(3),
     i(4),
     i(0)
   }))
 })
 


  local wk = require("which-key")

  -- Which-key mappings
  wk.add({
    -- Explorer
    { "<leader>e", "<cmd>Neotree toggle<cr>", desc = "Explorer" },

    -- Files
    { "<leader>f", group = "Files" },
    { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find File" },
    { "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "Recent Files" },
    { "<leader>fs", "<cmd>w<cr>", desc = "Save File" },
    { "<leader>fq", "<cmd>q<cr>", desc = "Quit File" },

    -- Git
    { "<leader>g", group = "Git" },
    { "<leader>gs", "<cmd>Git status<cr>", desc = "Status" },
    { "<leader>gc", "<cmd>Git commit<cr>", desc = "Commit" },
    { "<leader>gp", "<cmd>Git push<cr>", desc = "Push" },

    -- LSP
    { "<leader>l", group = "LSP" },
    { "<leader>lr", "<cmd>lua vim.lsp.buf.rename()<cr>", desc = "Rename" },
    { "<leader>lf", "<cmd>lua vim.lsp.buf.format({ async = true })<cr>", desc = "Format" },
    { "<leader>la", "<cmd>lua vim.lsp.buf.code_action()<cr>", desc = "Code Action" },
    { "<leader>ld", "<cmd>lua vim.lsp.buf.definition()<cr>", desc = "Go To Definition" },

    -- Windows
    { "<leader>w", group = "Windows" },
    { "<leader>ws", "<cmd>split<cr>", desc = "Horizontal Split" },
    { "<leader>wv", "<cmd>vsplit<cr>", desc = "Vertical Split" },
    { "<leader>wn", "<cmd>tabnew<cr>", desc = "New Tab" },
    { "<leader>wd", "<cmd>close<cr>", desc = "Close Window" },
    { "<C-h>", "<C-w>h", desc = "Move to Left Window" },
    { "<C-j>", "<C-w>j", desc = "Move to Below Window" },
    { "<C-k>", "<C-w>k", desc = "Move to Above Window" },
    { "<C-l>", "<C-w>l", desc = "Move to Right Window" },

    -- Terminal
    { "<leader>t", group = "Terminal" },
    { "<leader>tt", "<cmd>ToggleTerm<cr>", desc = "Toggle Terminal" },
  })

  -- LSP Setup for languages
  vim.lsp.config.pyright = {}
  vim.lsp.config.rust_analyzer = {}
  vim.lsp.config.lua_ls = {}
  vim.lsp.config.nil_ls = {}
  vim.lsp.config.nixd = {}
  vim.lsp.config.ts_ls = {
    filetypes = { "javascript", "javascriptreact", "typescript", "typescriptreact" }
  }
  vim.lsp.config.marksman = {
    filetypes = { "markdown", "mdx" }
  }
  vim.lsp.enable({"pyright", "rust_analyzer", "lua_ls", "nil_ls", "nixd", "ts_ls", "marksman" })
''
