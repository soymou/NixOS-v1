{
  config,
  lib,
  pkgs,
  ...
}: let
  nordVpnPkg = pkgs.callPackage ({
    autoPatchelfHook,
    buildFHSEnvChroot,
    dpkg,
    fetchurl,
    lib,
    stdenv,
    sysctl,
    iptables,
    iproute2,
    procps,
    cacert,
    libnl,
    libcap_ng,
    libxml2,
    libidn2,
    zlib,
    wireguard-tools,
  }: let
    pname = "nordvpn";
    version = "4.0.0";
    nordVPNBase = stdenv.mkDerivation {
      inherit pname version;
      src = fetchurl {
        url = "https://repo.nordvpn.com/deb/nordvpn/debian/pool/main/nordvpn_${version}_amd64.deb";
        hash = "sha256-elKREKiFrx2TgJPJl1ARtEebsv4PNG9fMq2mrV9xngs=";
      };
      buildInputs = [libxml2 libidn2 libnl libcap_ng];
      nativeBuildInputs = [dpkg autoPatchelfHook stdenv.cc.cc.lib];
      dontConfigure = true;
      dontBuild = true;
      unpackPhase = ''
        runHook preUnpack
        dpkg --extract $src .
        runHook postUnpack
      '';
      installPhase = ''
        runHook preInstall
        mkdir -p $out
        mv usr/* $out/
        mv var/ $out/
        mv etc/ $out/
        runHook postInstall
      '';
    };
    nordVPNfhs = buildFHSEnvChroot {
      name = "nordvpnd";
      runScript = "nordvpnd";
      targetPkgs = pkgs: [
        nordVPNBase
        sysctl
        iptables
        iproute2
        procps
        cacert
        libnl
        libcap_ng
        libxml2
        libidn2
        zlib
        wireguard-tools
      ];
    };
  in
    stdenv.mkDerivation {
      inherit pname version;
      dontUnpack = true;
      dontConfigure = true;
      dontBuild = true;
      installPhase = ''
        runHook preInstall
        mkdir -p $out/bin $out/share
        ln -s ${nordVPNBase}/bin/nordvpn $out/bin
        ln -s ${nordVPNfhs}/bin/nordvpnd $out/bin
        ln -s ${nordVPNBase}/share/* $out/share/
        ln -s ${nordVPNBase}/var $out/
        runHook postInstall
      '';
      meta = with lib; {
        description = "CLI client for NordVPN";
        homepage = "https://www.nordvpn.com";
        license = licenses.unfreeRedistributable;
        maintainers = with maintainers; [dr460nf1r3];
        platforms = ["x86_64-linux"];
      };
    }) {};
in
  with lib; {
    options.mou.services.custom.nordvpn = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable the NordVPN daemon. Note that you'll have to set
          `networking.firewall.checkReversePath = false;`, add UDP 1194
          and TCP 443 to the list of allowed ports in the firewall and add your
          user to the "nordvpn" group (`users.users.<username>.extraGroups`).
        '';
      };
      autoConnect = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to automatically connect to double VPN on boot.
          Requires NordVPN to be logged in beforehand.
        '';
      };
    };
    config = mkIf config.mou.services.custom.nordvpn.enable {
      networking.firewall.checkReversePath = false;
      environment.systemPackages = [nordVpnPkg];
      users.groups.nordvpn = {};
      users.groups.nordvpn.members = ["mou"];
      systemd = {
        services.nordvpn = {
          description = "NordVPN daemon.";
          serviceConfig = {
            ExecStart = "${nordVpnPkg}/bin/nordvpnd";
            ExecStartPre = pkgs.writeShellScript "nordvpn-start" ''
              mkdir -m 700 -p /var/lib/nordvpn;
              if [ -z "$(ls -A /var/lib/nordvpn)" ]; then
                cp -r ${nordVpnPkg}/var/lib/nordvpn/* /var/lib/nordvpn;
              fi
            '';
            NonBlocking = true;
            KillMode = "process";
            Restart = "on-failure";
            RestartSec = 5;
            RuntimeDirectory = "nordvpn";
            RuntimeDirectoryMode = "0750";
            Group = "nordvpn";
          };
          wantedBy = ["multi-user.target"];
          after = ["network-online.target"];
          wants = ["network-online.target"];
        };

        services.nordvpn-autoconnect = mkIf config.mou.services.custom.nordvpn.autoConnect {
          description = "NordVPN Auto-Connect to Double VPN";
          serviceConfig = {
            Type = "oneshot";
            User = "mou";
            Group = "nordvpn";
            ExecStart = pkgs.writeShellScript "nordvpn-autoconnect" ''
              # Wait for nordvpn daemon to be ready
              timeout=60
              while [ $timeout -gt 0 ]; do
                if ${nordVpnPkg}/bin/nordvpn status >/dev/null 2>&1; then
                  break
                fi
                sleep 1
                timeout=$((timeout - 1))
              done
              
              if [ $timeout -eq 0 ]; then
                echo "Timeout waiting for NordVPN daemon to be ready"
                exit 1
              fi
              
              # Check if already connected
              if ${nordVpnPkg}/bin/nordvpn status | grep -q "Status: Connected"; then
                echo "Already connected to VPN"
                exit 0
              fi
              
              # Connect to double VPN
              echo "Connecting to NordVPN Double VPN..."
              ${nordVpnPkg}/bin/nordvpn connect double_vpn
            '';
            RemainAfterExit = true;
          };
          wantedBy = ["multi-user.target"];
          after = ["nordvpn.service" "network-online.target"];
          requires = ["nordvpn.service"];
          wants = ["network-online.target"];
        };
      };
    };
  }
