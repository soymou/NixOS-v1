{ pkgs, ... }:

{
  home.packages = import ./pkgs/nix-packages.nix { inherit pkgs; };

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: import ./pkgs/emacs-packages.nix { inherit epkgs pkgs; };
    extraConfig = builtins.concatStringsSep "\n" [
      (builtins.readFile ./config/core.el)
      (builtins.readFile ./config/evil-config.el)
      (builtins.readFile ./config/ui.el)
      (builtins.readFile ./config/completion.el)
      (builtins.readFile ./config/dev.el)
      (builtins.readFile ./config/lang.el)
      (builtins.readFile ./config/org-config.el)
      (builtins.readFile ./config/ai.el)
    ];

  };

  home.file.".emacs.d/snippets".source = ./snippets;
}
