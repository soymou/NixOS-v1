{ config, pkgs, vars, inputs, ... }:

{
  imports = [
    inputs.illogical-flake.homeManagerModules.default
  ];

  programs.illogical-impulse = {
    enable = true;

    dotfiles = {
      fish.enable = true;
      kitty.enable = true;
      starship.enable = true;
    };

    hyprland.plugins = [
      inputs.hyprland-plugins.packages.${pkgs.stdenv.hostPlatform.system}.hyprscrolling
    ];
  };

    home.pointerCursor = {
    gtk = {
      enable = true;
      };
    # x11.enable = true;
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = 16;
  };

  gtk = {
    enable = true;

    theme = {
      package = pkgs.flat-remix-gtk;
      name = "Flat-Remix-GTK-Grey-Darkest";
    };

    iconTheme = {
      package = pkgs.adwaita-icon-theme;
      name = "Adwaita";
    };

    font = {
      name = "Sans";
      size = 11;
    };
  };
}
