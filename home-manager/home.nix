{ pkgs, outputs, inputs, lib, ... }:
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
    outputs.homeManagerModules.nyxt
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

      # Icon themes for KDE/Qt applications
      libsForQt5.breeze-icons
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
    settings = {
      user.name  = "soymou";
      user.email = "emilio.junoy@gmail.com";
    }; 
  };

  programs.nyxt = {
    enable = true;
    package = pkgs.symlinkJoin {
      name = "nyxt-wayland-fix";
      paths = [ pkgs.nyxt ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/nyxt \
          --set GDK_BACKEND x11 \
          --set NO_AT_BRIDGE 1 \
          --set WEBKIT_DISABLE_COMPOSITING_MODE 1
      '';
    };
  };

  systemd.user.startServices = "sd-switch";

  # GTK and Qt icon theme configuration
  gtk = {
    enable = true;
    iconTheme = {
      name = "breeze";
      package = pkgs.libsForQt5.breeze-icons;
    };
  };

  # Qt configuration for KDE applications
  qt = {
    enable = true;
    platformTheme.name = "gtk";
  };

  # Update icon cache after profile changes
  home.activation.updateIconCache = lib.hm.dag.entryAfter ["writeBoundary"] ''
    for icons_dir in "$HOME/.local/share/icons" "/etc/profiles/per-user/$USER/share/icons"; do
      if [ -d "$icons_dir" ]; then
        for theme_dir in "$icons_dir"/*; do
          if [ -d "$theme_dir" ] && [ "$(basename "$theme_dir")" != "default" ]; then
            $DRY_RUN_CMD ${pkgs.gtk3}/bin/gtk-update-icon-cache -f -t "$theme_dir" 2>/dev/null || true
          fi
        done
      fi
    done
  '';
}
