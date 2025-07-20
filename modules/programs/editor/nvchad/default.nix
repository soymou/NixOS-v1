{
  inputs,
  pkgs,
  ...
}: {
  home-manager.sharedModules = [
    (_: {
      imports = [inputs.nvchad4nix.homeManagerModule];
      programs.nvchad = {
        enable = true;

        extraPlugins = ''
          return {
            {
              "lervag/vimtex",
            },
          }
        '';

        extraPackages = with pkgs; [
          nixd
          lua-language-server
          # otros que necesites
        ];

        hm-activation = true;
        backup = false;
      };
    })
  ];
}

