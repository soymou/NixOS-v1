{ config, pkgs, vars, ... }:
{
  users.users."${vars.username}" = {
    isNormalUser = true;
    description = "${vars.username}";
    extraGroups = [ "networkmanager" "wheel" "video" "audio" ];
    packages = with pkgs; [
      # thunderbird
    ];
  };
}
