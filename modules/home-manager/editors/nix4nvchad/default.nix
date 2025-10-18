{ inputs, config, pkgs, ... }:
{
  imports = [
    inputs.nix4nvchad.homeManagerModules.default
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
    
    hm-activation = true;
  };
}
