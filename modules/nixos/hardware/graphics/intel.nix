{ config, pkgs, ... }:

{
  # Enable Xorg and the Intel driver
  services.xserver.videoDrivers = [ "intel" ];

  # Common OpenGL setup for Intel
  hardware.graphics = {
    enable = true;
    # Add Intel-specific OpenGL/VA-API packages
    extraPackages = with pkgs; [
      intel-media-driver
      vaapiIntel
      # libvdpau-va-gl # For VA-API on older hardware
    ];
  };

  # For some Intel chipsets, you might need specific kernel modules
  # boot.kernelModules = [ "i915" ];
}
