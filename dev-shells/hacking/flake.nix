{
  description = "Hacking dev-shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  };

  outputs = { self, nixpkgs, ... }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { 
    	inherit system;
	config.allowUnfree = true;
    };
  in {
    devShells."${system}".default = pkgs.mkShell {
      packages = with pkgs; [
        # Basic tools
	      dnsutils
        nmap
	      caido
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
        seclists
        dnsrecon
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
        exploitdb
        python3
        nodejs
        jython

        androidenv.androidPkgs.platform-tools
        jadx
        apktool
        android-studio
      ];

      shellHook = ''
        # Create symlink to seclists in the dev-shell directory
        SECLISTS_LINK="$PWD/seclists"
        if [ ! -e "$SECLISTS_LINK" ]; then
          ln -s ${pkgs.seclists}/share/wordlists/seclists "$SECLISTS_LINK"
          echo "Created symlink: seclists -> ${pkgs.seclists}/share/wordlists/seclists"
        fi

        JYTHON_LINK="$PWD/jython.jar"
        if [ ! -e "$JYTHON_LINK" ]; then
          ln -s ${pkgs.jython}/jython.jar "$JYTHON_LINK"
          echo "Created symlink: jython.jar ${pkgs.jython}/jython.jar"
        fi
      '';
    };
  };
}
