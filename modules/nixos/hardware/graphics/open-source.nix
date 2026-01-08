{ config, pkgs, ... }:

{
  # Enable Xorg and the generic modesetting driver
  services.xserver.videoDrivers = [ "modesetting" ];

  # Common OpenGL setup for open-source drivers
  hardware.graphics = {
    enable = true;
    # No specific extra packages needed for generic open-source
  };
}
