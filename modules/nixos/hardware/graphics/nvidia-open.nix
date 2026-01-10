{ config, pkgs, ... }:

{
  # Enable Xorg and the open-source NVIDIA driver
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.nvidia = {
    # Enable the open-source kernel module
    open = true;
    # Modesetting is required for most modern NVIDIA setups
    modesetting.enable = true;

    # Required for proper suspend/resume on NVIDIA (saves VRAM)
    powerManagement.enable = true;
    powerManagement.finegrained = false;

    # Enable `nvidia-settings` utility (even with open drivers, some tools are proprietary)
    # nvidiaSettings = true;

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
