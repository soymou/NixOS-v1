{ pkgs }:
with pkgs; [
  lua-language-server
  gcc
  nil
  nixd
  marksman
  pyright
  rust-analyzer
  nodePackages.typescript-language-server
]
