{pkgs, inputs, ... }:

{
  imports = [
    # ./example.nix - add your modules here
    ./nvchad/nvchad.nix
    ./Hacking/default.nix
    ./kitty/kitty.nix
    ./mime/mime.nix
  ];

  # home-manager options go here
  home.packages = with pkgs; [
    # pkgs.vscode - hydenix's vscode version
    # pkgs.userPkgs.vscode - your personal nixpkgs version
    inputs.zen-browser.packages."${system}".default
    warp-terminal
    gpt4all
    kitty
  ];

  # Enable direnv
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };
  
  # hydenix home-manager options go here
  hydenix.hm = {
    #! Important options
    enable = true;
      #! Below are defaults, no need to uncomment them all
      comma.enable = true; # useful nix tool to run software without installing it first
      dolphin.enable = true; # file manager
      editors = {
        enable = true; # enable editors module
        neovim = true; # enable neovim module
        default = "nvim"; # default text editor
      };
      fastfetch.enable = true; # fastfetch configuration
      firefox.enable = true; # enable firefox module
      git = {
        enable = true; # enable git module
        name = "emilio-junoy";
        email = "emilio.junoy@gmail.com";
      };
      hyde.enable = true; # enable hyde module
      hyprland = {
        enable = true; # enable hyprland module
        extraConfig = "
          input {
            kb_layout = us
            kb_variant = intl
          }
          monitor=HDMI-A-1,1920x1080@60,0x0,1.25
          bind = SUPER, RETURN, exec, $TERMINAL
          bind = SUPER, SPACE, exec, pkill -x rofi || /home/mou/.local/lib/hyde/rofilaunch.sh d
        "; # extra hyprland config text
      };
      lockscreen = {
        enable = true; # enable lockscreen module
        hyprlock = true; # enable hyprlock lockscreen
        swaylock = false; # enable swaylock lockscreen
      };
      notifications.enable = true; # enable notifications module
      qt.enable = true; # enable qt module
      rofi.enable = true; # enable rofi module
      screenshots = {
        enable = true; # enable screenshots module
        grim.enable = true; # enable grim screenshot tool
        slurp.enable = true; # enable slurp region selection tool
        satty.enable = false; # enable satty screenshot annotation tool
        swappy.enable = true; # enable swappy screenshot editor
      };
      shell = {
        enable = true; # enable shell module
        zsh = {
          enable = true; # enable zsh shell
          plugins = [ "sudo" ]; # zsh plugins
          configText = ""; # zsh config text
        };
        bash.enable = false; # enable bash shell
        fish.enable = false; # enable fish shell
        pokego.enable = false; # enable Pokemon ASCII art scripts
        p10k.enable = false; # enable p10k prompt
        starship.enable = true; # enable starship prompt
      };
      social = {
        enable = true; # enable social module
        discord.enable = true; # enable discord module
        webcord.enable = true; # enable webcord module
        vesktop.enable = true; # enable vesktop module
      };
      spotify.enable = true; # enable spotify module
      swww.enable = true; # enable swww wallpaper daemon
      terminals = {
        enable = false; # enable terminals module
        kitty = {
          enable = false; # enable kitty terminal
          configText = "
            cursor_trail 0
          "; # kitty config text
        };
      };
      theme = {
        enable = true; # enable theme module
        active = "Gruvbox-Retro"; # active theme name
        themes = [
          "Catppuccin Mocha"
          "Catppuccin Latte"
	        "Hack-the-Box"
	        "Gruvbox-Retro"
        ]; # default enabled themes, full list in https://github.com/richen604/hydenix/tree/main/hydenix/sources/themes
      };
      waybar = {
        enable = true; # enable waybar module
        userStyle = ""; # custom waybar user-style.css
      };
      wlogout.enable = true; # enable wlogout module
      xdg.enable = false;
    };
}
