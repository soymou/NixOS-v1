{ pkgs, ... }:
let 
  custom-emacs = pkgs.emacsWithPackagesFromUsePackage {
      package = pkgs.emacs-unstable;
      config = ./config/init.el;
      
      override = epkgs: epkgs // {
            ob-lean4 = pkgs.callPackage ./config/packages/ob-lean4.nix {
              inherit (pkgs) fetchFromGitHub;
              inherit (epkgs) trivialBuild;
            };
            lean4-mode = pkgs.callPackage ./config/packages/lean4-mode.nix {
              inherit (pkgs) fetchFromGitHub;
              inherit (epkgs) trivialBuild magit-section dash lsp-mode;
            };
	        };  

      extraEmacsPackages = (import ./config/packages/emacs-packages.nix);
    };

in
{
  services.emacs = {
    enable = true;
    package = custom-emacs;
  };
 
  programs.emacs = {
    enable = true;
    package = custom-emacs;
  };
}
