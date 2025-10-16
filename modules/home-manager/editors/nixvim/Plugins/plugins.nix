{pkgs}:
let
  dashboardConfig = import ../Dashboard/dashboard.nix;
in
{
  lualine.enable = true;
  treesitter = {
    enable = true;
    grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
      markdown
      markdown_inline
      tsx
      norg
    ];
  };
  treesitter-textobjects.enable = true;
  treesitter-context.enable = true;

  neorg = {
    enable = true;
    settings = {
      load = {
        "core.concealer" = {
          icon_preset = "varied";
        };

        "core.defaults" = {
          __emtpy = null;
        };

        "core.dirman" = {
          config = {
            workspaces = {
              home = "~/notas";
            };
          };
        };

        "core.keybinds" = {
          config = {
            default_keybinds = true;
          };
        };
      };
    };
  };

  blink-cmp = {
    enable = true;
    settings = {
      sources = {
        default = ["lsp" "path" "snippets" "buffer"];
      };
      snippets.preset = "luasnip";
      keymap = {
        preset = "enter";
        "<Tab>" = ["select_next" "snippet_forward" "fallback"];
        "<S-Tab>" = ["select_prev" "snippet_backward" "fallback"];
      };
    };
  };
  neo-tree.enable = true;
  which-key.enable = true;
  web-devicons.enable = true;
  telescope.enable = true;
  mini.enable = true;
  toggleterm.enable = true;
  indent-blankline.enable = true;
  lspconfig.enable = true;
  luasnip = {
    enable = true;
    fromVscode = [
      { lazyLoad = true; }
    ];
  };
  friendly-snippets.enable = true;

  # Formatting
  conform-nvim.enable = true;

  # Git
  gitsigns.enable = true;
  fugitive.enable = true;

  # Code helpers
  comment.enable = true;
  nvim-autopairs.enable = true;
  nvim-surround.enable = true;

  # Navigation
  flash.enable = true;
  harpoon.enable = true;
  oil.enable = true;

  # UI
  noice.enable = true;
  trouble.enable = true;
  bufferline.enable = false;

  # Utilities
  undotree.enable = true;
  todo-comments.enable = true;


  alpha = dashboardConfig;
}
