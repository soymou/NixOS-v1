{ pkgs, inputs, ... }:

{
  home.packages = with pkgs; [
    inputs.burpsuitepro.packages."${system}".default
    dirbuster
    nmap
    zenmap
    seclists
    bettercap
    ettercap
    metasploit
    theharvester
    sherlock
    wireshark-qt
  ];
}

