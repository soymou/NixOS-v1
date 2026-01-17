{ config, pkgs, inputs, ... }:
{
  programs.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
  };

  fonts.packages = with pkgs; [
    rubik
    nerd-fonts.ubuntu
    nerd-fonts.jetbrains-mono
  ];
}
