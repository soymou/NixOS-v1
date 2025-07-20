
{ inputs, pkgs, ... }:

{
  home-manager.sharedModules = [
    (_: {
      home.packages = with pkgs; [
        zathura
        zathura-pdf-mupdf # or zathura-pdf-poppler for PDF support
      ];

      # Optional: Custom config file
      xdg.configFile."zathura/zathurarc".text = ''
        set selection-clipboard clipboard
        set adjust-open "best-fit"
        set recolor true
        set recolor-keephue true
        set statusbar-h-padding 2
        set statusbar-v-padding 2
        set font "monospace 10"
      '';
    })
  ];
}
