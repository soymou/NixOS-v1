{ config, pkgs, ... }:

{
  services.tor = {
    enable = true;

    settings = {
      SocksPort = [ "127.0.0.1:9050" ];

      Log = [ "notice stdout" ];
    };
  };
}
