{
  inputs,
  pkgs,
  ...
}: {
  home-manager.sharedModules = [
    inputs.nixvim.homeManagerModules.default
    (_: {
      programs.nixvim = {
        enable = true;

        extraPlugins = with pkgs.vimPlugins; [
          vimtex
        ];

        extraConfigLua = ''
          vim.g.tex_flavor = "latex"
          vim.g.vimtex_quickfix_mode = 0
          vim.g.vimtex_mappings_enabled = 0
          vim.g.vimtex_indent_enabled = 0
          vim.g.vimtex_view_method = "zathura"
          vim.g.vimtex_context_pdf_viewer = "zathura"
        '';
      };

      home.packages = with pkgs; [
        texlive.combined.scheme-full
        zathura
      ];
    })
  ];
}

