{ config, pkgs, ... }:
{
  programs.hyprland = {
    enable = true;
  };

  fonts.packages = with pkgs; [
    rubik
    nerd-fonts.ubuntu
    nerd-fonts.jetbrains-mono
  ];
}
