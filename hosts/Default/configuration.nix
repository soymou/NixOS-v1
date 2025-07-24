{
  pkgs,
  videoDriver,
  hostname,
  browser,
  editor,
  terminal,
  terminalFileManager,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    ../../modules/hardware/video/${videoDriver}.nix
    ../../modules/hardware/drives

    ../common.nix
    ../../modules/scripts

    ../../modules/desktop/hyprland
    # ../../modules/desktop/i3-gaps

    ../../modules/programs/games
    ../../modules/programs/browser/${browser}
    ../../modules/programs/terminal/${terminal}
    ../../modules/programs/editor/${editor}
    ../../modules/programs/cli/${terminalFileManager}
    ../../modules/programs/cli/starship
    ../../modules/programs/cli/tmux
    ../../modules/programs/cli/direnv
    ../../modules/programs/cli/lazygit
    ../../modules/programs/cli/cava
    ../../modules/programs/cli/btop
    ../../modules/programs/shell/bash
    ../../modules/programs/shell/zsh
    ../../modules/programs/media/discord
    ../../modules/programs/media/spicetify
    # ../../modules/programs/media/youtube-music
    # ../../modules/programs/media/thunderbird
    # ../../modules/programs/media/obs-studio
    ../../modules/programs/media/mpv
    ../../modules/programs/misc/tlp
    ../../modules/programs/misc/thunar
    ../../modules/programs/misc/lact
    # ../../modules/programs/misc/nix-ld
    # ../../modules/programs/misc/virt-manager
    ../../modules/programs/vpn/nordvpn
  ];

  home-manager.sharedModules = [
    (_: {
      home.packages = with pkgs; [
        github-desktop
        lua-language-server
        tree-sitter
        ollama
        zathura
        obsidian
        caido
        burpsuite
        dirbuster
        nmap
        zenmap
        metasploit
        seclists
        wireshark
        firefox
      ];
    })
  ];

  environment.systemPackages = with pkgs; [
  ];

  networking.hostName = hostname;

  services.minidlna = {
    enable = true;
    openFirewall = true;
    settings = {
      friendly_name = "NixOS-DLNA";
      media_dir = [
        "/mnt/work/Pimsleur"
        "/mnt/work/Media/Films"
        "/mnt/work/Media/Series"
        "/mnt/work/Media/Videos"
        "/mnt/work/Media/Music"
      ];
      inotify = "yes";
      log_level = "error";
    };
  };

  users.users.minidlna = {
    extraGroups = ["users"];
  };

  mou.services.custom.nordvpn.enable = true;
  mou.services.custom.nordvpn.autoConnect = true;

  users.groups.wireshark = {};
  programs.wireshark.enable = true;

  users.users.mou = {
    isNormalUser = true;
    extraGroups = [ "nordvpn" "networkmanager" "wireshark" ];
  };

  networking.firewall.allowedUDPPorts = [ 1194 ];
  networking.firewall.allowedTCPPorts = [ 443 ];
  networking.firewall.checkReversePath = false;

  # === Tor Service Enablement ===
  services.tor = {
    enable = true;
    settings = {
      SocksPort = [ "9050" ]; # default SOCKS5 port
      Log = [ "notice stdout" ];
    };
  };
}

