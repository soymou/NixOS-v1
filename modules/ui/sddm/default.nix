{ pkgs, ... }: 
{
  services.displayManager.sddm = {
    enable = true;
    package = pkgs.kdePackages.sddm;
    theme = "sddm-astronaut-theme";
    extraPackages = with pkgs; [
      sddm-astronaut # Add the theme package itself
      kdePackages.qtmultimedia # Include necessary Qt multimedia package
      kdePackages.qtsvg # Include necessary Qt SVG package
      kdePackages.qtvirtualkeyboard # Include virtual keyboard package
    ];
  };
  
  environment.systemPackages = with pkgs; [
    sddm-astronaut
    kdePackages.qtmultimedia
    kdePackages.qtsvg
    kdePackages.qtvirtualkeyboard
  ];
}
