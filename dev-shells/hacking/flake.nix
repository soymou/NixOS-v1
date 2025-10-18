{
  description = "Hacking dev-shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  };

  outputs = { self, nixpkgs, ... }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
  in {
    devShells."${system}".default = pkgs.mkShell {
      packages = with pkgs; [
        # Basic tools
        nmap 
        nikto
        tcpdump
        dnsmap
        zap 
        bettercap
        aircrack-ng
        sherlock
        theharvester
        armitage
        sqlmap
        kismet
        medusa
        wifite2
        ffuf
        zenmap 
        dirbuster 
        gobuster
        wireshark
        hydra 
        john
        hashcat
        go
        nuclei
        subfinder
        metasploit
        python3
      ];
    };
  };
}
