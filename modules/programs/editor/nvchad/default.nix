{
 inputs,
  pkgs,
  lib,
  ...
}: let
  luaModules = [ "luasnip" "treesitter" "texlab" "lean" "neogit" "chadrc" "gp"];

  luaRequireStatements = builtins.concatStringsSep "\n" (lib.mapAttrsToList (_: module: ''
    require('config.${module}')
  '') (lib.listToAttrs (map (m: { name = m; value = m; }) luaModules)));

  snippetFiles =
    let
      entries = builtins.readDir ./snippets;
    in
      lib.filterAttrs (name: type: lib.hasSuffix ".lua" name && type == "regular") entries;

in {
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
            { "Robitx/gp.nvim", lazy = false },
          }
        '';

        extraPackages = with pkgs; [ nixd lua-language-server ];

        extraConfig = ''
          ${luaRequireStatements}
        '';

        hm-activation = true;
        backup = false;
      };

      # Generate home.file entries for config modules + all snippet files
      home.file =
        (builtins.foldl' (acc: module:
          acc // {
            ".config/nvim/lua/config/${module}.lua".source = ./lua/config/${module}.lua;
          }
        ) {} luaModules)
        //
        (lib.mapAttrs' (name: _: {
          name = ".config/nvim/snippets/${name}";
          value = { source = ./snippets/${name}; };
        }) snippetFiles);

    })
  ];
}
