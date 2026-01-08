{ config, pkgs, vars, ... }: 
{
  boot = {
    # 1. Custom Visuals
    plymouth = {
      enable = true;
      theme = "breeze"; 
    };

    # 2. Advanced GRUB Theming
    loader = {
      timeout = 5;
      grub = {
        enable = true;
        efiSupport = true;
        useOSProber = true;
        configurationLimit = 10; # Added for safety
        device = "nodev";

        # Installs a nice NixOS theme for GRUB
        theme = pkgs.nixos-grub2-theme;
        
        # Ensures the boot menu looks good on HiDPI/4K screens
        gfxmodeEfi = "1920x1080";
      };
      
      efi.canTouchEfiVariables = true;
    };

    # 3. Installer Safety Net
    kernelParams = [ "quiet" "splash" "boot.shell_on_fail" ];
    tmp.cleanOnBoot = true;

    # 4. Use latest kernel
    kernelPackages = pkgs.linuxPackages_latest;
  };

  # 5. Console font for the TTY (Pre-GUI)
  console = {
    earlySetup = true;
    font = "ter-v16n";
    packages = with pkgs; [ terminus_font ];
  };
}
