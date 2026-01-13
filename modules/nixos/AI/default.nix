{ config, pkgs, ... }:
{
  services.ollama = {
    enable = true;
    port = 12345;
    host = "0.0.0.0";
    openFirewall = true;
    package = pkgs.ollama-cuda;
    loadModels = [
      "ministral-3:14b"
    ];
  };

  systemd.services.ollama.wantedBy = [ "multi-user.target" ];

  services.nextjs-ollama-llm-ui.enable = true;
}
