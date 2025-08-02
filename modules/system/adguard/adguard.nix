
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
            "9.9.9.9#dns.quad9.net"
            "149.112.112.112#dns.quad9.net"
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
        ];
      };
    };
  };
}

