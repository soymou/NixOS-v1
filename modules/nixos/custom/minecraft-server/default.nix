{ config, pkgs, inputs, ... }:
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
