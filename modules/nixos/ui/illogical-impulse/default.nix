{ config, lib, pkgs, inputs, ... }:
let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types;

  cfg = config.services.illogical-impulse;
  
  # External packages
  nurPkgs = inputs.nur.legacyPackages.${pkgs.system};
  
  # Python environment for quickshell wallpaper analysis
  pythonEnv = pkgs.python3.withPackages (ps: [
    ps.build
    ps.cffi
    ps.click
    ps."dbus-python"
    ps."kde-material-you-colors"
    ps.libsass
    ps.loguru
    ps."material-color-utilities"
    ps.materialyoucolor
    ps.numpy
    ps.pillow
    ps.psutil
    ps.pycairo
    ps.pygobject3
    ps.pywayland
    ps.setproctitle
    ps."setuptools-scm"
    ps.tqdm
    ps.wheel
    ps."pyproject-hooks"
    ps.opencv4
  ]);
in
{
  options.services.illogical-impulse = {
    enable = mkEnableOption "Enable the Illogical Impulse Hyprland setup";

    user = mkOption {
      type = types.str;
      description = "User to configure for Illogical Impulse";
    };

    dotfilesRoot = mkOption {
      type = types.str;
      default = "/home/${cfg.user}/dots-hyprland/dots";
      description = ''
        Location of the Illogical Impulse dotfiles repository.
      '';
    };

    hyprland = {
      enable = mkEnableOption "Enable Hyprland window manager" // { default = true; };
      
      monitor = mkOption {
        type = types.listOf types.str;
        default = [ ",preferred,auto,1" ];
        description = "Monitor preferences passed to Hyprland.";
      };

      package = mkOption {
        type = types.package;
        default = inputs.hyprland.packages.${pkgs.system}.hyprland;
        description = "Hyprland package to use.";
      };

      xdgPortalPackage = mkOption {
        type = types.package;
        default = inputs.hyprland.packages.${pkgs.system}.xdg-desktop-portal-hyprland;
        description = "xdg-desktop-portal implementation for Hyprland.";
      };

      ozoneWayland.enable = mkEnableOption "Set NIXOS_OZONE_WL=1 for Chromium based apps.";
    };

    dotfiles = {
      kitty.enable = mkEnableOption "Install kitty and use the Illogical Impulse kitty config.";
      fish.enable = (mkEnableOption "Use the Illogical Impulse fish config.") // { default = true; };
      starship.enable = mkEnableOption "Install starship and use the Illogical Impulse prompt.";
    };
  };

  config = mkIf cfg.enable {
    # Enable required services
    services.xserver.enable = true;
    services.displayManager.sddm.enable = true;
    services.displayManager.sddm.wayland.enable = true;
    
    # Hyprland configuration with portal
    programs.hyprland = mkIf cfg.hyprland.enable {
      enable = true;
      package = cfg.hyprland.package;
      xwayland.enable = true;
      portalPackage = cfg.hyprland.xdgPortalPackage;
    };

    # Audio
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
    };

    # Network Manager
    networking.networkmanager.enable = true;

    # Bluetooth
    hardware.bluetooth.enable = true;
    hardware.bluetooth.powerOnBoot = true;
    services.blueman.enable = true;

    # Required fonts
    fonts.packages = with pkgs; [
      material-symbols
      rubik
      nurPkgs.repos.skiletro.gabarito
      nerd-fonts.ubuntu
      nerd-fonts.ubuntu-mono
      nerd-fonts.jetbrains-mono
      nerd-fonts.caskaydia-cove
      nerd-fonts.fantasque-sans-mono
      nerd-fonts.mononoki
      nerd-fonts.space-mono
    ];

    # System packages
    environment.systemPackages = [
      # QuickShell
      inputs.quickshell.packages.${pkgs.system}.default
      
      # Core utilities
      pkgs.cava
      pkgs.lxqt.pavucontrol-qt
      pkgs.wireplumber
      pkgs.libdbusmenu-gtk3
      pkgs.playerctl
      pkgs.brightnessctl
      pkgs.ddcutil
      pkgs.axel
      pkgs.bc
      pkgs.cliphist
      pkgs.curl
      pkgs.rsync
      pkgs.wget
      pkgs.libqalculate
      pkgs.ripgrep
      pkgs.jq
      
      # GUI applications
      pkgs.foot
      pkgs.fuzzel
      pkgs.matugen
      pkgs.mpv
      pkgs.mpvpaper
      pkgs.swappy
      pkgs.wf-recorder
      pkgs.hyprshot
      pkgs.wlogout
      
      # System utilities
      pkgs.xdg-user-dirs
      pkgs.tesseract
      pkgs.slurp
      pkgs.upower
      pkgs.wtype
      pkgs.ydotool
      pkgs.glib
      pkgs.swww
      pkgs.translate-shell
      pkgs.hyprpicker
      
      # Wayland/Hyprland specific
      pkgs.hyprlock
      pkgs.hypridle
      pkgs.hyprsunset
      pkgs.redshift
      pkgs.swayidle
      pkgs.wayland-protocols
      pkgs.wl-clipboard
      
      # Development libraries
      pkgs.libsoup_3
      pkgs.libportal-gtk4
      pkgs.gobject-introspection
      pkgs.sassc
      pkgs.opencv
      
      # Themes and icons
      pkgs.adw-gtk3
      pkgs.illogical-impulse-oneui4-icons
      
      # Python with required packages for wallpaper analysis
      pythonEnv
      pkgs.python313Packages.kde-material-you-colors
      pkgs.eza
      
      # KDE/Qt packages (required for QuickShell)
      pkgs.gnome-keyring
      pkgs.kdePackages.bluedevil
      pkgs.kdePackages.bluez-qt
      pkgs.kdePackages.plasma-nm
      pkgs.kdePackages.polkit-kde-agent-1
      pkgs.networkmanager
      pkgs.kdePackages.kcmutils
      pkgs.kdePackages.plasma-workspace
      pkgs.kdePackages.systemsettings
      pkgs.kdePackages.kdialog
      
      # Qt packages for QuickShell functionality
      pkgs.kdePackages.qt5compat      # Visual effects (blur, etc.)
      pkgs.kdePackages.qtbase
      pkgs.kdePackages.qtdeclarative
      pkgs.kdePackages.qtimageformats # WEBP and other image formats
      pkgs.kdePackages.qtmultimedia   # Media playbook
      pkgs.kdePackages.qtpositioning
      pkgs.kdePackages.qtquicktimeline
      pkgs.kdePackages.qtsensors
      pkgs.kdePackages.qtsvg          # SVG image support
      pkgs.kdePackages.qttools
      pkgs.kdePackages.qttranslations
      pkgs.kdePackages.qtvirtualkeyboard
      pkgs.kdePackages.qtwayland
      pkgs.kdePackages.qtwebsockets
      pkgs.kdePackages.syntax-highlighting
      
      # Additional Qt support
      pkgs.libsForQt5.qtgraphicaleffects
      pkgs.libsForQt5.qtsvg
    ] ++ lib.optionals cfg.dotfiles.fish.enable [
      pkgs.fish
    ] ++ lib.optionals cfg.dotfiles.kitty.enable [
      pkgs.kitty
    ] ++ lib.optionals cfg.dotfiles.starship.enable [
      pkgs.starship
    ];

    # Environment variables
    environment.sessionVariables = {
      QT_QPA_PLATFORMTHEME = "kde";
      QT_STYLE_OVERRIDE = "";
    } // lib.optionalAttrs cfg.hyprland.ozoneWayland.enable {
      NIXOS_OZONE_WL = "1";
    };

    # User configuration
    users.users.${cfg.user} = {
      isNormalUser = true;
      extraGroups = [ "wheel" "networkmanager" "audio" "video" ];
      shell = if cfg.dotfiles.fish.enable then pkgs.fish else pkgs.bash;
    };

    # Fish shell configuration
    programs.fish.enable = cfg.dotfiles.fish.enable;
  };
}
