{ config, pkgs, vars, ... }:
{
  services.guix = {
    enable = true;
  };
  environment.systemPackages = with pkgs; [
    neofetch
    gcc
  ];
}

