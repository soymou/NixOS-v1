{
  inputs,
  pkgs,
  ...
}:
let
  plugins = import ./Plugins/plugins.nix { inherit pkgs; };
  extraPlugins = import ./Plugins/extraPlugins.nix { inherit pkgs; };
  extraPackages = import ./Packages/extraPackages.nix { inherit pkgs; };
  luaConfig = import ./Config/lua-config.nix;
  opts = import ./Config/opts.nix;
  spellDir = ./spell;
in {
  imports = [
    inputs.nixvim.homeModules.nixvim
  ];

  programs.nixvim = {
    enable = true;

    globals.mapleader = " ";
    colorschemes.nord.enable = true;

    plugins = plugins;
    extraPlugins = extraPlugins;
    extraPackages = extraPackages;
    opts = opts;
    extraConfigLua = luaConfig;

    extraFiles."spell/es.utf-8.spl".source = "${spellDir}/es.utf-8.spl";
  };
}
