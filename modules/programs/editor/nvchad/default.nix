{
  inputs,
  pkgs,
  ...
}: {
  home-manager.sharedModules = [
    (_: {
      imports = [ inputs.nvchad4nix.homeManagerModule ];

      programs.nvchad = {
        enable = true;

        extraPlugins = ''
          return {
            { "lervag/vimtex", ft = "tex", lazy = true },
            { "Julian/lean.nvim", ft = "lean", lazy = true },
            { "neovim/nvim-lspconfig", lazy = false},
            { "L3MON4D3/LuaSnip", lazy = false},
            { "TimUntersberger/neogit", lazy = false},
          }
        '';

        extraPackages = with pkgs; [
          nixd
          lua-language-server
        ];

        extraConfig = ''
          -- Configure snippets
          
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
          -- Configure Tree-sitter to work with vimtex concealment
          require'nvim-treesitter.configs'.setup {
          highlight = {
            enable = true,
            -- IMPORTANT: Enable additional regex highlighting for LaTeX
            additional_vim_regex_highlighting = { "latex" },
          },
         }
          vim.cmd('syntax enable')
          vim.api.nvim_create_autocmd("FileType", {
            pattern = "tex",
            callback = function()
              vim.opt_local.conceallevel = 2
              vim.opt_local.concealcursor = "c"
            end,
          })

         -- Configure texlab LSP
          local lspconfig = require("lspconfig")
          lspconfig.texlab.setup({
            on_attach = function(client, bufnr)
              -- Optional: Customize keybindings or settings here
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
              },
            },
          })
          -- Simple Lean setup
          require('lean').setup({
          abbreviations = { builtin = true },
          infoview = { autoopen = true },
          })
        '';

        hm-activation = true;
        backup = false;
      };
      home.file.".config/nvim/snippets/tex.lua".source = ./snippets/tex.lua;
    })
  ];
}

