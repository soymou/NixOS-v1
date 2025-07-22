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
          }
        '';

        extraPackages = with pkgs; [
          nixd
          lua-language-server
        ];

        extraConfig = ''
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
    })
  ];
}

