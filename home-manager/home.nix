{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    outputs.homeManagerModules.illogical-impulse
  ];


  illogical-impulse = {
    enable = true;
    dotfiles = {
      fish.enable = true;
      kitty.enable = true;
      starship.enable = true;
    };
  };

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
      nodejs
    ];
  };

  programs.home-manager.enable = true;
  
  programs.git = {
    enable    = true;
    userName  = "soymou";
    userEmail = "emilio.junoy@gmail.com";
  };

  programs.neovim.enable = true;
  
  systemd.user.startServices = "sd-switch";

  home.stateVersion = "25.05";
}
