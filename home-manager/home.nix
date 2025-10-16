{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  system,
  ...
}: {
  imports = [
    outputs.homeManagerModules.nixvim
    outputs.homeManagerModules.zen-browser
    outputs.homeManagerModules.minecraft
  ];

  # illogical-impulse is now configured as a NixOS module

  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
    ];
    config = {
      allowUnfree = true;
    };
  };

  home = {
    username = "mou";
    homeDirectory = "/home/mou";
    
    packages = with pkgs; [
      claude-code
      discord
      spotify
      zathura
    ];
   
    pointerCursor = {
      name = "Bibata-Modern-Ice";
      package = pkgs.bibata-cursors;
      size = 24;
    };

  };

  programs.home-manager.enable = true;

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.git = {
    enable    = true;
    userName  = "soymou";
    userEmail = "emilio.junoy@gmail.com";
  };
  
  systemd.user.startServices = "sd-switch";

  home.stateVersion = "25.05";
}
