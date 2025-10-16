{ config, lib, pkgs, inputs, ... }:
let
  inherit (lib)
    mkEnableOption
    mkIf
    mkMerge
    mkOption
    optionals
    types
    concatStringsSep;

  cfg = config.illogical-impulse;

  dotfilesRoot = cfg.dotfilesRoot;
  mkDotfile = rel: config.lib.file.mkOutOfStoreSymlink "${dotfilesRoot}${rel}";

  hyprlandCfg = cfg.hyprland;
  hyprlandPackage = hyprlandCfg.package;
  hyprlandPortal = hyprlandCfg.xdgPortalPackage;

  nurPkgs = inputs.nur.legacyPackages.${pkgs.system};
  qt6 = pkgs.qt6Packages or pkgs.kdePackages;
  unstablePkgs = inputs.nixpkgs-unstable.legacyPackages.${pkgs.system};
  unstableQt6 = unstablePkgs.qt6Packages or {};
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
  pythonSitePackages = pkgs.python3.sitePackages;
  qt5CompatGraphicalEffects =
    if qt6 ? qt5compatgraphicaleffects then qt6.qt5compatgraphicaleffects
    else null;
  qtGraphicalEffectsPkg =
    if qt5CompatGraphicalEffects != null then qt5CompatGraphicalEffects
    else if qt6 ? qtgraphicaleffects then qt6.qtgraphicaleffects
    else if unstableQt6 ? qt5compatgraphicaleffects then unstableQt6.qt5compatgraphicaleffects
    else pkgs.qt6.qtwebview;
  quickshellPackage = inputs.quickshell.packages.${pkgs.system}.default;
  quickshellWrapped = pkgs.symlinkJoin {
    name = "quickshell-wrapped";
    paths = [ quickshellPackage ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      for bin in "$out"/bin/*; do
        if [ -f "$bin" ] && [ -x "$bin" ]; then
          wrapProgram "$bin" \
            --set QML_IMPORT_PATH '${quickshellQmlPath}' \
            --set QML2_IMPORT_PATH '${quickshellQmlPath}';
        fi
      done
    '';
  };

  unstableQtImport = if unstableQt6 ? qt5compat then "${unstableQt6.qt5compat}/lib/qt-6/qml" else null;

  quickshellQmlPath = concatStringsSep ":" (
    [
      "${qt6.qt5compat}/lib/qt-6/qml"
      "${qtGraphicalEffectsPkg}/lib/qt-6/qml"
      "${qt6.qtpositioning}/lib/qt-6/qml"
      "${qt6.qtwebsockets}/lib/qt-6/qml"
      "${qt6.qtmultimedia}/lib/qt-6/qml"
      "${qt6.qtimageformats}/lib/qt-6/qml"
      "${pkgs.kdePackages.plasma-nm}/lib/qt-6/qml"
      "${pkgs.kdePackages.syntax-highlighting}/lib/qt-6/qml"
      "${pkgs.kdePackages.bluedevil}/lib/qt-6/qml"
      "${pkgs.kdePackages.bluez-qt}/lib/qt-6/qml"
    ]
    ++ lib.optional (unstableQtImport != null) unstableQtImport
  );
in
{
  options.illogical-impulse = {
    enable = mkEnableOption "Enable the Illogical Impulse Hyprland setup";

    dotfilesRoot = mkOption {
      type = types.str;
      default = config.home.homeDirectory + "/dots-hyprland/dots";
      description = ''
        Location of the Illogical Impulse dotfiles repository. This should be a
        string path (for example, "/home/mou/dots-hyprland/dots") and will be
        linked into place using out-of-store symlinks.
      '';
    };

    hyprland = {
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

  config = mkIf cfg.enable (
    mkMerge [
      {
        xdg.configFile."Kvantum/Colloid".source = mkDotfile "/.config/Kvantum/Colloid";
        xdg.configFile."matugen".source = mkDotfile "/.config/matugen";
        xdg.configFile."mpv/mpv.conf".source = mkDotfile "/.config/mpv/mpv.conf";
      }

      (mkIf cfg.dotfiles.fish.enable {
        xdg.configFile."fish/config.fish".source = mkDotfile "/.config/fish/config.fish";
        home.packages = [ pkgs.fish ];
      })

      (mkIf cfg.dotfiles.kitty.enable {
        xdg.configFile."kitty".source = mkDotfile "/.config/kitty";
        home.packages = [ pkgs.kitty ];
      })

      (mkIf cfg.dotfiles.starship.enable {
        xdg.configFile."starship.toml".source = mkDotfile "/.config/starship.toml";
        home.packages = [ pkgs.starship ];
      })

      {
        gtk = {
          enable = true;
          iconTheme = {
            package = pkgs.illogical-impulse-oneui4-icons;
            name = "OneUI";
          };
        };

        qt = {
          enable = true;
          platformTheme.name = "kde6";
        };

        home.sessionVariables = {
          ILLOGICAL_IMPULSE_VIRTUAL_ENV = "~/.local/state/quickshell/.venv";
          QML_IMPORT_PATH = quickshellQmlPath;
          QML2_IMPORT_PATH = quickshellQmlPath;
          QT_QPA_PLATFORMTHEME = "kde";
          QT_STYLE_OVERRIDE = "";
        };
        
        home.sessionPath = [
          "${config.home.homeDirectory}/.local/bin"
        ];

        home.packages =
          (with pkgs; [
            cava
            lxqt.pavucontrol-qt
            wireplumber
            libdbusmenu-gtk3
            playerctl
            brightnessctl
            ddcutil
            axel
            bc
            cliphist
            curl
            rsync
            wget
            libqalculate
            ripgrep
            jq
            foot
            fuzzel
            matugen
            mpv
            mpvpaper
            xdg-user-dirs
            adw-gtk3
            eza
            python313Packages.kde-material-you-colors
            material-symbols
            rubik
            wl-clipboard
            kdePackages.bluedevil
            kdePackages.bluez-qt
            gnome-keyring
            kdePackages.plasma-nm
            kdePackages.polkit-kde-agent-1
            networkmanager
            kdePackages.kcmutils
            kdePackages.plasma-workspace
            kdePackages.systemsettings
            swappy
            wf-recorder
            hyprshot
            tesseract
            slurp
            upower
            wtype
            ydotool
            libsoup_3
            libportal-gtk4
            gobject-introspection
            sassc
            opencv
            (python3.withPackages (python-pkgs:
              with python-pkgs; [
                build
                pillow
                setuptools-scm
                wheel
                pywayland
                psutil
                materialyoucolor
                libsass
                material-color-utilities
                setproctitle
              ]))
            glib
            swww
            translate-shell
            wlogout
            hyprpicker
            hyprlock
            hypridle
            hyprsunset
            redshift
            swayidle
            wayland-protocols
            quickshellWrapped
            qtGraphicalEffectsPkg
            kdePackages.kdialog
            kdePackages.qt5compat
            kdePackages.qtbase
            kdePackages.qtdeclarative
            kdePackages.qtimageformats
            kdePackages.qtmultimedia
            kdePackages.qtpositioning
            kdePackages.qtquicktimeline
            kdePackages.qtsensors
            kdePackages.qtsvg
            kdePackages.qttools
            kdePackages.qttranslations
            kdePackages.qtvirtualkeyboard
            kdePackages.qtwayland
            kdePackages.qtwebsockets
            kdePackages.syntax-highlighting
            libsForQt5.qtgraphicaleffects
            libsForQt5.qtsvg
          ])
          ++ (with pkgs.nerd-fonts; [
            ubuntu
            ubuntu-mono
            jetbrains-mono
            caskaydia-cove
            fantasque-sans-mono
            mononoki
            space-mono
          ])
          ++ [
            nurPkgs.repos.skiletro.gabarito
            pkgs.illogical-impulse-oneui4-icons
          ];

        services = {
          gammastep = {
            enable = true;
            provider = "geoclue2";
          };
          network-manager-applet.enable = true;
        };

        wayland.windowManager.hyprland = {
          enable = true;
          systemd.enable = false;
          xwayland.enable = true;
          package = hyprlandPackage;
          portalPackage = hyprlandPortal;

          settings = {
            "$qsConfig" = "ii";

            env =
              [
                "GIO_EXTRA_MODULES, ${pkgs.gvfs}/lib/gio/modules:$GIO_EXTRA_MODULES"
              ]
              ++ optionals hyprlandCfg.ozoneWayland.enable [
                "NIXOS_OZONE_WL, 1"
              ];
            exec = [
              "hyprctl dispatch submap global"
            ];
            submap = "global";
            debug.disable_logs = false;
            monitor = hyprlandCfg.monitor;
          };

          extraConfig = ''
            # Defaults
            source=~/.config/hypr/hyprland/execs.conf
            source=~/.config/hypr/hyprland/general.conf
            source=~/.config/hypr/hyprland/rules.conf
            source=~/.config/hypr/hyprland/colors.conf
            source=~/.config/hypr/hyprland/keybinds.conf

            # Custom 
            source=~/.config/hypr/custom/env.conf
            source=~/.config/hypr/custom/execs.conf
            source=~/.config/hypr/custom/general.conf
            source=~/.config/hypr/custom/rules.conf
            source=~/.config/hypr/custom/keybinds.conf
          '';
        };

        services.hypridle = {
          enable = true;
          settings = {
            general = {
              lock_cmd = "pidof hyprlock || hyprlock";
              before_sleep_cmd = "loginctl lock-session";
            };

            listener = [
              {
                timeout = 120;
                on-timeout = "loginctl lock-session";
              }
              {
                timeout = 600;
                on-timeout = "hyprctl dispatch dpms off";
                on-resume = "hyprctl dispatch dpms on";
              }
              {
                timeout = 900;
                on-timeout = "systemctl suspend || loginctl suspend";
              }
            ];
          };
        };

        xdg.configFile = {
          "quickshell".source = mkDotfile "/.config/quickshell";
          "hypr/hyprland/scripts".source = mkDotfile "/.config/hypr/hyprland/scripts";
          "hypr/hyprland/execs.conf".source = mkDotfile "/.config/hypr/hyprland/execs.conf";
          "hypr/hyprland/general.conf".source = mkDotfile "/.config/hypr/hyprland/general.conf";
          "hypr/hyprland/rules.conf".source = mkDotfile "/.config/hypr/hyprland/rules.conf";
          "hypr/hyprland/keybinds.conf".source = mkDotfile "/.config/hypr/hyprland/keybinds.conf";
          "hypr/hyprland/env.conf".source = mkDotfile "/.config/hypr/hyprland/env.conf";
          "hypr/hyprland/colors.conf".source = mkDotfile "/.config/hypr/hyprland/colors.conf";
          "hypr/hyprland.conf".source = mkDotfile "/.config/hypr/hyprland.conf";
          "hypr/hyprlock".source = mkDotfile "/.config/hypr/hyprlock";
          "hypr/shaders".source = mkDotfile "/.config/hypr/shaders";
          "hypr/custom".source = mkDotfile "/.config/hypr/custom";
          "hypr/workspaces.conf".source = mkDotfile "/.config/hypr/workspaces.conf";
          "hypr/monitors.conf".source = mkDotfile "/.config/hypr/monitors.conf";
        };
      }

      {
        home.activation.illogicalImpulseConfig = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
          # Copy config.json instead of symlinking it for proper FileView monitoring
          CONFIG_DIR="${config.home.homeDirectory}/.config/illogical-impulse"
          CONFIG_SOURCE="${dotfilesRoot}/.config/illogical-impulse/config.json"
          CONFIG_TARGET="$CONFIG_DIR/config.json"
          
          mkdir -p "$CONFIG_DIR"
          if [ -L "$CONFIG_TARGET" ] || [ ! -f "$CONFIG_TARGET" ] || [ "$CONFIG_SOURCE" -nt "$CONFIG_TARGET" ]; then
            rm -f "$CONFIG_TARGET"
            cp "$CONFIG_SOURCE" "$CONFIG_TARGET"
            echo "Copied config.json to enable proper file monitoring"
          fi
          
          # Ensure correct quickshell/qs commands are available in ~/.local/bin
          mkdir -p "${config.home.homeDirectory}/.local/bin"
          ln -sf ${quickshellWrapped}/bin/quickshell "${config.home.homeDirectory}/.local/bin/quickshell"
          ln -sf ${quickshellWrapped}/bin/qs "${config.home.homeDirectory}/.local/bin/qs"
        '';
        
        home.activation.illogicalImpulseVenv = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
          VENV="${config.home.homeDirectory}/.local/state/quickshell/.venv"
          mkdir -p "$VENV/bin"

          ln -sf ${pythonEnv}/bin/python3 "$VENV/bin/python"
          ln -sf ${pythonEnv}/bin/python3 "$VENV/bin/python3"
          ln -sf ${pythonEnv}/bin/pip "$VENV/bin/pip"

          cat > "$VENV/bin/activate" <<'EOF'
_II_PREV_PATH="$PATH"
_II_PREV_PYTHONPATH="$PYTHONPATH"
export VIRTUAL_ENV="${config.home.homeDirectory}/.local/state/quickshell/.venv"
export PATH="${pythonEnv}/bin:$PATH"
export PYTHONPATH="${pythonEnv}/${pythonSitePackages}:${pythonEnv}/lib/python3.12/site-packages"
deactivate() {
  PATH="$_II_PREV_PATH"
  export PATH
  if [ -n "$_II_PREV_PYTHONPATH" ]; then
    PYTHONPATH="$_II_PREV_PYTHONPATH"
    export PYTHONPATH
  else
    unset PYTHONPATH
  fi
  unset VIRTUAL_ENV
  unset _II_PREV_PATH
  unset _II_PREV_PYTHONPATH
  unset -f deactivate
}
EOF
          chmod +x "$VENV/bin/activate"

          TARGET_DIR="${config.home.homeDirectory}/.local/state/quickshell/user/generated/terminal"
          if [ -d "$TARGET_DIR" ]; then
            ${pkgs.coreutils}/bin/chmod u+w "$TARGET_DIR"
            ${pkgs.findutils}/bin/find "$TARGET_DIR" -type f -exec ${pkgs.coreutils}/bin/chmod u+w {} +
          fi
        '';
      }
    ]
  );
}
