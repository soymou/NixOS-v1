{ epkgs, pkgs }:
with epkgs; [
  magit
  doom-themes
  doom-modeline
  vertico
  marginalia
  orderless
  consult
  which-key
  rainbow-delimiters
  nix-mode
  clojure-mode
  cider
  rustic
  envrc
  ob-async
  tramp
  ob-rust

  # Added packages
  restclient
  ob-restclient
  ob-rust
  gcmh
  general
  evil
  evil-collection
  vundo
  rainbow-mode
  centaur-tabs
  dashboard
  nerd-icons
  nerd-icons-dired
  nerd-icons-corfu
  helpful
  corfu
  projectile
  lsp-mode
  lsp-ui
  lsp-treemacs
  consult-lsp
  (import ./lean4-mode.nix { inherit pkgs epkgs; })
  (import ./ob-lean4.nix { inherit pkgs epkgs; })
  org-modern
  org-appear
  yasnippet
  flycheck
  dash
  s
  f

  # Latex
  auctex
  cdlatex
  pdf-tools

  # New additions (Navigation, Dev, Org, UI)
  avy
  dirvish
  vterm
  vterm-toggle
  diff-hl
  apheleia
  org-roam
  org-download
  dimmer
  eat

  # Tree view
  treemacs
  treemacs-evil
  treemacs-projectile
  treemacs-nerd-icons


  # Ai
  (import ./gemini-cli.nix { inherit pkgs epkgs; })
  (import ./ai-code.nix { inherit pkgs epkgs; })
]
