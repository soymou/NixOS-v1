{ config, pkgs, inputs, ... }:
let
  world = import /home/mou/Desktop/Personal/Minecraft;
in
{
  imports = [
    inputs.nix-minecraft.nixosModules.minecraft-servers
  ];

  services.minecraft-servers = {
    enable = false;
    eula = true;
    openFirewall = true;
    };
  };
}
