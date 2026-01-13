{ config, pkgs, inputs, ... }:
let
  world = import /home/mou/Desktop/Personal/Minecraft;
in
{
  imports = [
    inputs.nix-minecraft.nixosModules.minecraft-servers
  ];

  services.minecraft-servers = {
    enable = true;
    eula = true;
    openFirewall = true;
    servers.Squigs = {
      enable = true;
      package = pkgs.fabricServers.fabric-1_21_11.override {
        loaderVersion = "0.18.4";
      };
      serverProperties = {
        level-name = "Squigs";
        difficulty = "peaceful";
      };
    };
  };
}
