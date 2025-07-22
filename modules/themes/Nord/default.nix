{ pkgs, wallpaper, ... }: {
  home-manager.sharedModules = [
    ({ config, ... }: {
      services.hyprpaper.enable = true;
      services.hyprpaper.settings = {
        preload = ["${../wallpapers/${wallpaper}.jxl}"];
        wallpaper = [",${../wallpapers/${wallpaper}.jxl}"];
      };

      dconf.settings = {
        "org/gnome/desktop/interface" = {
          gtk-theme = "Nordic";
          color-scheme = "prefer-dark";
        };
        # add your Nordic GNOME Terminal dconf here...
      };

      home.pointerCursor = {
        gtk.enable = true;
        x11.enable = true;
        package = pkgs.bibata‑cursors;
        name = "Bibata‑Modern‑Classic";
        size = 16;
      };

      qt.enable = true;
      qt.platformTheme.name = "gtk";
      # optionally import Kvantum nord theme...

      gtk.enable = true;
      gtk.theme.name = "Nordic";
      gtk.theme.package = pkgs.nordic;
      gtk.iconTheme.name = "Nordyz";
      gtk.iconTheme.package = pkgs.nordzy-icon-theme;
      gtk.gtk3.extraConfig."gtk-application-prefer-dark-theme" = "1";
      gtk.gtk4.extraConfig."gtk-application-prefer-dark-theme" = "1";

      xdg.configFile."gtk-4.0/assets".source = "${config.gtk.theme.package}/share/themes/Nordic/gtk-4.0/assets";
      xdg.configFile."gtk-4.0/gtk.css".source = "${config.gtk.theme.package}/share/themes/Nordic/gtk-4.0/gtk.css";
      xdg.configFile."gtk-4.0/gtk-dark.css".source = "${config.gtk.theme.package}/share/themes/Nordic/gtk-4.0/gtk-dark.css";
    })
  ];
}
