{ pkgs, ... }:

{
  imports = [
    # ./example.nix - add your modules here
    ./vpn/nordvpn.nix
    ./adguard/adguard.nix 
  ];

  environment.systemPackages = with pkgs; [
    # pkgs.vscode - hydenix's vscode version
    # pkgs.userPkgs.vscode - your personal nixpkgs version 
    gcc
    direnv
  ];
}
