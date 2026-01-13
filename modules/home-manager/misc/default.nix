{ config, pkgs, inputs, ... }:
let 
  burpsuite = inputs.burpsuitepro.packages.${pkgs.stdenv.hostPlatform.system}.default;
in
{

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  home.packages = with pkgs; [
    burpsuite
    discord
    spotify
    gemini-cli
    lmstudio
    vscode
    neovim
    protonvpn-gui
    zathura
    tor
  ]; 
}
