{ config, pkgs ,vars, ... }:
{
  networking.hostName = "${vars.hostName}";
  networking.networkmanager.enable = true;

  networking.firewall.allowedTCPPorts = [ 9050 ];  
}
