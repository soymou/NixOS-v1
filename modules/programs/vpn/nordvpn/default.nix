{ config, pkgs, ...} : {
  environment.systemPackages = with pkgs; [
    nur.repos.nltch.nordvpn
  ];

  networking.networkmanager.enable = true;

  users.groups.nordvpn = {};
  users.users.mou.extraGroups = ["nordvpn"];

  systemd.services.nordvpn = {
    enable = true;
    wantedBy = ["multi-user.target"];
  };

  networking.firewall.checkReversePath = false;
}

