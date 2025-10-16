{ inputs, pkgs, ... }:
let
  modpack-squigs = pkgs.fetchPackwizModpack {
    url = "https://raw.githubusercontent.com/soymou/Minecraft/refs/heads/main/Server/pack.toml";
    packHash = "sha256-KTgxW12JZUlRxX9itMxuEPo8+MCA/t/62qqbcv4F9SY=";
  };
  modpack-cogo = pkgs.fetchPackwizModpack {
    url = "https://raw.githubusercontent.com/soymou/Minecraft/refs/heads/main/Cogo/pack.toml";
    packHash = "sha256-QgtekX3uZx/XKoTGNDEid9ZeJ0s9RFFmuAaVoA8dAHs=";
  };
in
{
  imports = [ inputs.nix-minecraft.nixosModules.minecraft-servers ];
  
  services.minecraft-servers = {
    enable = true;
    eula = true;
    openFirewall = true;
    servers = { 
      squigs = {
        enable = true;
        package = pkgs.fabricServers.fabric-1_20_1;
        serverProperties = {
          server-port = 25565;
          difficulty = "peaceful";
          gamemode = "survival";
        };
        symlinks = {
          "mods" = "${modpack-squigs}/mods";
        };
        jvmOpts = [
          "-Xmx12G"
        ];
      };
      
      Locos = {
        enable = true;
        package = pkgs.fabricServers.fabric-1_20_1;
        serverProperties = {
          server-port = 25566;
          difficulty = "normal";
          gamemode = "survival";
        };
        symlinks = {
          "mods" = "${modpack-cogo}/mods";
        };
        jvmOpts = [
          "-Xmx12G"
        ];
      };

    };
  };

}
