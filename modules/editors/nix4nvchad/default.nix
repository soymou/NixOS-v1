{ inputs, config, pkgs, ... }:
{
  imports = [
    inputs.nix4nvchad.homeMangerModule
  ]; 

  programs.nvchad = {
    enable = true;
    extraPackages = with pkgs; [
      nodePackages.bash-language-server
      nixd 
      (python3.withPackages(ps: with ps; [
        python-lsp-server
      ]))
    ];
    
    extraPlugins = builtins.readFile ./plugins/extraPlugins.lua;
    
    hm-activation = true;
  };
}
