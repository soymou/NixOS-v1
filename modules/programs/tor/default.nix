{ config, pkgs, ... }:

{
  services.tor = {
    enable = true;
    configFile = ./torrc; 
  };
}
