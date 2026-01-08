{ config, pkgs, ... }:

{
  # Enable Xorg and the proprietary NVIDIA driver
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.nvidia = {
    # Modesetting is required for most modern NVIDIA setups
    modesetting.enable = true;

    # You may want to enable power management for laptops, but test carefully
    powerManagement.enable = false;
    powerManagement.finegrained = false;

    # Enable `nvidia-settings` utility
    nvidiaSettings = true;

    # For NVIDIA Optimus laptops (hybrid graphics)
    # prime = {
    #   offload.enable = true;
    #   sync.enable = true;
    #   # Replace with your Intel integrated GPU bus ID
    #   # intelBusId = "PCI:0:2:0";
    #   # Replace with your NVIDIA discrete GPU bus ID
    #   # nvidiaBusId = "PCI:1:0:0";
    # };
  };

  # Common OpenGL setup for NVIDIA
  hardware.graphics = {
    enable = true;
    # For NVIDIA, you generally don't need extraPackages unless for specific applications
  };
}
