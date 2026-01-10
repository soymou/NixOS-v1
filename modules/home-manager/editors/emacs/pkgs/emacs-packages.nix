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
  envrc
  ob-async
  tramp

  # Added packages
  restclient
  ob-restclient
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
  (import ./lean4-mode.nix { inherit pkgs epkgs; })
  org-modern
  org-appear
  yasnippet
  flycheck
  dash
  s
  f

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

  # Ai
  (import ./gemini-cli.nix { inherit pkgs epkgs; })
  (import ./ai-code.nix { inherit pkgs epkgs; })
]
