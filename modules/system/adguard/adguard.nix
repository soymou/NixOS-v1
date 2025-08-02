{ config, lib, pkgs, ... }:

{
  options.my.adguard.enable = lib.mkEnableOption "Enable AdGuard Home with default filters";

  config = lib.mkIf config.my.adguard.enable {
    services.adguardhome = {
      enable = true;
      settings = {
        http.address = "0.0.0.0:3000";
        dns = {
          bind_hosts = [ "0.0.0.0" ]; 
          port = 53;
          upstream_dns = [
            "8.8.8.8"
            "8.8.4.4"
          ];
        };
        filtering = {
          protection_enabled = true;
          filtering_enabled = true;
          parental_enabled = false;
          safe_search.enabled = false;
        };
        filters = map (url: { enabled = true; url = url; }) [
          "https://adguardteam.github.io/HostlistsRegistry/assets/filter_9.txt"
          "https://adguardteam.github.io/HostlistsRegistry/assets/filter_11.txt"
          "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
          "https://phishing.army/download/phishing_army_blocklist_extended.txt"
          "https://s3.amazonaws.com/lists.disconnect.me/simple_tracking.txt"
        ]; 
      };
    };
  };
}

