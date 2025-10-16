{
  enable = true;
  layout = [
    { type = "padding"; val = { __raw = "vim.fn.max({ 2, vim.fn.floor(vim.fn.winheight(0) * 0.2) })"; }; }
    { type = "text"; val = [
        "███╗   ██╗██╗██╗  ██╗██╗   ██╗██╗███╗   ███╗"
        "████╗  ██║██║╚██╗██╔╝██║   ██║██║████╗ ████║"
        "██╔██╗ ██║██║ ╚███╔╝ ██║   ██║██║██╔████╔██║"
        "██║╚██╗██║██║ ██╔██╗ ╚██╗ ██╔╝██║██║╚██╔╝██║"
        "██║ ╚████║██║██╔╝ ██╗ ╚████╔╝ ██║██║ ╚═╝ ██║"
        "╚═╝  ╚═══╝╚═╝╚═╝  ╚═╝  ╚═══╝  ╚═╝╚═╝     ╚═╝"
      ]; opts = { hl = "Type"; position = "center"; }; }
    { type = "padding"; val = 1; }
    { type = "padding"; val = 2; }
    {
      type = "group";
      val = {
        __raw = ''
          (function()
            local dashboard = require('alpha.themes.dashboard')
            return {
              dashboard.button("n", "  New File", ":ene <BAR> startinsert <CR>"),
              dashboard.button("f", "  Find File", ":Telescope find_files<CR>"),
              dashboard.button("r", "  Recent Files", ":Telescope oldfiles<CR>"),
              dashboard.button("e", "  File Browser", ":Neotree<CR>"),
              dashboard.button("c", "  Configuration", ":e ~/NixOS/modules/programs/editor/nixvim/default.nix<CR>"),
              dashboard.button("q", "  Quit", ":qa<CR>"),
            }
          end)()
        '';
      };
    }
    { type = "padding"; val = 1; }
    { type = "text"; val = "🚀 Happy Coding!"; opts = { hl = "Number"; position = "center"; }; }
  ];
}
