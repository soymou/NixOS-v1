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
      systemd-boot.enable = true;
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
