{ inputs, pkgs, ... }: {
  home-manager.sharedModules = [
    inputs.nixvim.homeManagerModules.default
    (_: {
      programs.nixvim = {
        enable = true;

        # Core settings
        opts = {
          number = true;
          relativenumber = true;
          tabstop = 2;
          softtabstop = 2;
          shiftwidth = 2;
          expandtab = true;
          smartindent = true;
          autoindent = true;
          ignorecase = true;
          smartcase = true;
          hlsearch = true;
          incsearch = true;
          termguicolors = true;
          signcolumn = "yes";
          wrap = false;
          scrolloff = 8;
          sidescrolloff = 8;
          cursorline = true;
          splitbelow = true;
          splitright = true;
          undofile = true;
          swapfile = false;
          backup = false;
          updatetime = 50;
          timeoutlen = 300;
          mouse = "a";
          clipboard = "unnamedplus";
          conceallevel = 2;
        };

        # Appearance
        colorschemes.tokyonight = {
          enable = true;
          settings = {
            style = "night";
            transparent = false;
            terminal_colors = true;
            styles = {
              comments = { italic = true; };
              keywords = { italic = true; };
              functions = { };
              variables = { };
            };
          };
        };

        # Plugins
        plugins = {
          neo-tree.enable = true;
          telescope = {
            enable = true;
            extensions.fzf-native.enable = true;
            extensions.ui-select.enable = true;
            settings.defaults = {
              file_ignore_patterns = [ "node_modules" ".git" "%.lock" "__pycache__" "dist/" ];
              set_env.COLORTERM = "truecolor";
            };
            keymaps = {
              "<leader>ff" = "find_files";
              "<leader>fg" = "live_grep";
              "<leader>fb" = "buffers";
              "<leader>fh" = "help_tags";
              "<leader>fr" = "oldfiles";
              "<leader>fc" = "grep_string";
            };
          };
          treesitter = {
            enable = true;
            settings = {
              highlight.enable = true;
              indent.enable = true;
              incremental_selection.enable = true;
              ensure_installed = [ ]; # Let devShells manage this
            };
          };
          comment.enable = true;
          nvim-autopairs.enable = true;
          nvim-surround.enable = true;
          gitsigns.enable = true;
          fugitive.enable = true;
          lualine = {
            enable = true;
            settings.options = {
              theme = "tokyonight";
              globalstatus = true;
            };
          };
          bufferline.enable = true;
          indent-blankline.enable = true;
          which-key.enable = true;
          web-devicons.enable = true;
         
          lean = {
            enable = true;
            settings = {
             lsp = {
                enable = true; 
              };  
            }; 
          };

          # LSP
          lsp = {
            enable = true;
            servers = {
              texlab.enable = true;
            };
          };

          # Completion
          cmp.enable = true;
          cmp-nvim-lsp.enable = true;
          cmp-buffer.enable = true;
          cmp-path.enable = true;
          cmp_luasnip.enable = true;
          luasnip.enable = true;

          # Formatting/linting (disabled)
          conform-nvim.enable = false;

          # Debugging
          dap.enable = true;
          dap-ui.enable = true;
          dap-virtual-text.enable = true;

          # Extras
          toggleterm.enable = true;
          leap.enable = true;
          auto-session.enable = true;
          todo-comments.enable = true;
          trouble.enable = true;
        };

        # Key mappings
        globals = {
          mapleader = " ";
          maplocalleader = ",";
        };

        keymaps = [
          { mode = "n"; key = "<leader>w"; action = "<cmd>w<cr>"; options = { desc = "Save"; }; }
          { mode = "n"; key = "<leader>q"; action = "<cmd>q<cr>"; options = { desc = "Quit"; }; }
          { mode = "n"; key = "<leader>x"; action = "<cmd>x<cr>"; options = { desc = "Save + Quit"; }; }
          { mode = "n"; key = "<leader>e"; action = "<cmd>Neotree toggle<cr>"; options = { desc = "Toggle file explorer"; }; }
        ];

        extraConfigLua = ''
          vim.g.tex_flavor = "latex"
          vim.g.vimtex_view_method = "zathura"

          vim.opt.foldmethod = "expr"
          vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
          vim.opt.foldenable = false

          local cmp = require("cmp")
          local luasnip = require("luasnip")
          local lspkind = require("lspkind")

          cmp.setup({
            snippet = {
              expand = function(args)
                luasnip.lsp_expand(args.body)
              end,
            },
            mapping = cmp.mapping.preset.insert({
              ["<C-Space>"] = cmp.mapping.complete(),
              ["<CR>"] = cmp.mapping.confirm({ select = true }),
              ["<Tab>"] = cmp.mapping.select_next_item(),
              ["<S-Tab>"] = cmp.mapping.select_prev_item(),
            }),
            sources = cmp.config.sources({
              { name = "nvim_lsp" },
              { name = "luasnip" },
              { name = "buffer" },
              { name = "path" },
            }),
            formatting = {
              format = lspkind.cmp_format({ mode = "symbol_text", maxwidth = 50 }),
            },
          })
        '';

        extraPlugins = with pkgs.vimPlugins; [vimtex lspkind-nvim];
      };

      home.packages = with pkgs; [
        ripgrep
        fd
        fzf
        git
        curl
        wget
        gnumake
        pkg-config
        nixpkgs-fmt
        stylua
        shellcheck
      ];
    })
  ];
}
