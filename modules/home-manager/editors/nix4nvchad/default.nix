{ inputs, config, pkgs, ... }:
{
  imports = [
    inputs.nix4nvchad.homeManagerModules.default
  ];

  programs.nvchad = {
    enable = true;
    extraPackages = with pkgs; [
      nodePackages.bash-language-server
      nixd
      (python3.withPackages(ps: with ps; [
        python-lsp-server
      ]))
      rust-analyzer
    ];

    hm-activation = true;
  };

  # Desktop entry for NvChad
  xdg.desktopEntries.nvim = {
    name = "Neovim by NvChad";
    genericName = "Text Editor";
    comment = "Edit text files";
    exec = "nvim %F";
    icon = "nvchad";  # Use nvchad icon instead of nvim
    terminal = true;
    type = "Application";
    categories = [ "Utility" "TextEditor" ];
    mimeType = [
      "text/english"
      "text/plain"
      "text/x-makefile"
      "text/x-c++hdr"
      "text/x-c++src"
      "text/x-chdr"
      "text/x-csrc"
      "text/x-java"
      "text/x-moc"
      "text/x-pascal"
      "text/x-tcl"
      "text/x-tex"
      "application/x-shellscript"
      "text/x-c"
      "text/x-c++"
    ];
  };
}
