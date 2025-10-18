# Add your reusable modules to this directory
# These should be stuff you would like to share with others, not your personal configurations.
{
  # Home Manager modules
  homeManagerModules = {
    #nixvim = import ./editors/nixvim;
    nvchad = import ./editors/nix4nvchad;
    zen-browser = import ./browsers/zen-browser;
    minecraft = import ./minecraft-home;
  };

  # NixOS modules
  nixosModules = {
    sddm = import ./ui/sddm;
    minecraft-servers = import ./minecraft-nixos;
  };
}
