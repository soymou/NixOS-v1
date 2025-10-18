{
  description = "Hacking dev-shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    burpsuitepro = {
      url = "github:soymou/Burpsuite-Professional";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, burpsuitepro, ... }: 
  let 
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
  in {
    devshells."${system}".default = pkgs.mkShell {
      packages = with pkgs; [
        # Basic tools 
        burpsuitepro
        nmap 
        nikto
        tcpdump
        dnsmap
        zap 
        massscan
        bettercap
        aircrack-ng
        sherlock
        theHarvester
        nessus
        armitage
        sqlmap
        kismet
        reaver
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
        python3.withPackages (packages: with packages; [
        ]) 
      ];
    };
  };
}
