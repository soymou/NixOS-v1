{ pkgs, lib, ... }:
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
  
  # Custom Emacs with packages
  custom-emacs = pkgs.emacs-unstable.pkgs.withPackages (epkgs: 
    (emacsPackages (customPackages epkgs))
  );
in
{
  programs.emacs = {
    enable = true;
    package = custom-emacs;
  };

  services.emacs = {
    enable = true;
    package = custom-emacs;
  };
  
  # Copy the modular configuration files to ~/.config/emacs/
  home.file.".config/emacs" = {
    source = ./config;
    recursive = true;
  };
  
  # Create init.el that loads the modular configuration
  home.file.".emacs.d/init.el".text = ''
    ;;; init.el --- Emacs Configuration Entry Point -*- lexical-binding: t; -*-

    ;;; Commentary:
    ;; This file loads the modular Emacs configuration from ~/.config/emacs/

    ;;; Code:

    ;; Don't auto-install packages - they're provided by Nix
    (setq use-package-always-ensure nil)

    ;; Add the configuration directories to load-path
    (let ((config-dir "~/.config/emacs/"))
      (add-to-list 'load-path config-dir)
      (add-to-list 'load-path (expand-file-name "modules" config-dir))
      (add-to-list 'load-path (expand-file-name "modules/core" config-dir))
      (add-to-list 'load-path (expand-file-name "modules/syntax" config-dir))
      (add-to-list 'load-path (expand-file-name "modules/languages" config-dir))
      (add-to-list 'load-path (expand-file-name "modules/org" config-dir))
      (add-to-list 'load-path (expand-file-name "modules/AI" config-dir)))

    ;; Load configuration modules
    (require 'modules/core/custom-evil)        ; Evil mode (Vim keybindings)
    (require 'modules/core/custom-keybinds)    ; Custom keybindings
    (require 'modules/core/custom-ui)          ; Basic UI improvements
    (require 'modules/core/custom-appearance)  ; Themes and modeline
    (require 'modules/syntax/custom-completion)  ; Ivy/Counsel completion
    (require 'modules/syntax/custom-treesit)     ; Tree-sitter configuration
    (require 'modules/syntax/custom-lsp)         ; Lsp config
    (require 'modules/syntax/custom-flycheck)    ; Flycheck config
    (require 'modules/syntax/custom-spell)       ; Enable spell checking
    (require 'modules/languages/custom-mdx)         ; MDX file support
    (require 'modules/org/custom-babel)       ; Load babel languages
    (require 'modules/org/custom-org)         ; Load org-mode configuration
    (require 'modules/AI/custom-aider)

    ;; Run startup apps
    (set-keybinds)
    (set-babel-languages languages)

    ;;; init.el ends here
  '';
  
  home.packages = packages; 
}
