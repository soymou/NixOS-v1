{ config, pkgs, nur, ... }:

{
  environment.systemPackages = with pkgs; [
    nur.repos.wingej0.nordvpn 
  ];

  networking.networkmanager.enable = true;

  users.groups.nordvpn = {};
  users.users.mou.extraGroups = [ "nordvpn" ];

  systemd.services.nordvpn = {
    enable = true;
    wantedBy = [ "multi-user.target" ];
  };

  networking.firewall.checkReversePath = false;
}

