{ config, pkgs, ... }:

{
  # Enable Xorg and the AMDGPU driver
  services.xserver.videoDrivers = [ "amdgpu" ];

  # Common OpenGL setup for AMD
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  }; 

  hardware.amdgpu.opencl.enable = true;

  # For systems with older AMD GPUs or specific needs, you might need firmware
  # hardware.firmware = [ pkgs.amdgpu_firmware ];
}
