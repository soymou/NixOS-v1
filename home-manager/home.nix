{ pkgs, outputs, inputs, ... }:
let
  system = "x86_64-linux";
  burpsuitepro = inputs.burpsuitepro.packages."${system}".default;
in
{
  imports = [
    #outputs.homeManagerModules.nixvim
    outputs.homeManagerModules.zen-browser
    outputs.homeManagerModules.minecraft
    outputs.homeManagerModules.nvchad
  ];

  home = {
    username = "mou";
    homeDirectory = "/home/mou";
    stateVersion = "25.05";

    packages = with pkgs; [
      claude-code
      discord
      spotify
      zathura
      fastfetch
      protonvpn-gui
      gemini-cli
      burpsuitepro
      jython
    ];

    pointerCursor = {
      name = "Bibata-Modern-Ice";
      package = pkgs.bibata-cursors;
      size = 24;
    };
  };

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
}
