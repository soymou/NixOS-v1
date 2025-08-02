{ inputs, pkgs, lib, config, ... }:

let
  luaModules = [ "neogit" "lsp"];

  luaRequireStatements = builtins.concatStringsSep "\n" (lib.mapAttrsToList (_: module: ''
    require('config.${module}')
  '') (lib.listToAttrs (map (m: { name = m; value = m; }) luaModules)));

  snippetFiles =
    let
      entries = builtins.readDir ./snippets;
    in
      lib.filterAttrs (name: type: lib.hasSuffix ".lua" name && type == "regular") entries;

in
{
  imports = [
    inputs.nix4nvchad.homeManagerModule
  ];

  programs.nvchad = {
    enable = true;

    extraPlugins = ''
      return {
        { "lervag/vimtex", ft = "tex", lazy = false },
        { "TimUntersberger/neogit", lazy = false }
      }
    '';

    extraPackages = with pkgs; [
      nixd
      lua-language-server
      bash-language-server
      nodePackages.bash-language-server
      docker-compose-language-service
      dockerfile-language-server-nodejs
      emmet-language-server
      (python3.withPackages (ps: with ps; [ python-lsp-server flake8 ]))
    ];

    extraConfig = ''
      ${luaRequireStatements}
    '';

    hm-activation = true;
    backup = false;
  };

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
}

