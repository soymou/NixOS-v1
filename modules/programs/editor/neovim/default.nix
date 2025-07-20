{ inputs, ... }: {
  home-manager.sharedModules = [
    (_: {
      programs.neovim.enable = true;
      home.file.".config/nvim" = {
        source = inputs.nvim-config;
        recursive = true;
      };
    })
  ];
}
