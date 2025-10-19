{ pkgs, lib, config, ... }:
let
  # Import the packages list
  emacsPackages = import ./config/packages/emacs-packages.nix;
  packages = import ./config/packages/packages.nix { inherit pkgs; };

  # Create custom packages
  customPackages = epkgs: epkgs // {
    ob-lean4 = pkgs.callPackage ./config/packages/ob-lean4.nix {
      inherit (pkgs) fetchFromGitHub;
      inherit (epkgs) trivialBuild;
    };
    lean4-mode = pkgs.callPackage ./config/packages/lean4-mode.nix {
      inherit (pkgs) fetchFromGitHub;
      inherit (epkgs) trivialBuild magit-section dash lsp-mode;
    };
  };
in
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-unstable.pkgs.withPackages (epkgs:
      (emacsPackages (customPackages epkgs))
    );
  };

  services.emacs = {
    enable = true;
    package = config.programs.emacs.finalPackage;
  };

  # Link the config directory to ~/.emacs.d
  home.file.".emacs.d/init.el".source = ./config/init.el;
  home.file.".emacs.d/modules".source = ./config/modules;

  home.packages = packages;
}
