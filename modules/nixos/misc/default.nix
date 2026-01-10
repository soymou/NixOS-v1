{ config, pkgs, vars, ... }:
{
  environment.systemPackages = with pkgs; [
    neofetch
    gcc
  ];
}

