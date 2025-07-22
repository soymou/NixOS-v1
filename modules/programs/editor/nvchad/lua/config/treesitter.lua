require'nvim-treesitter.configs'.setup {
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = { "latex" },
  },
}

vim.cmd('syntax enable')

vim.api.nvim_create_autocmd("FileType", {
  pattern = "tex",
  callback = function()
    vim.opt_local.conceallevel = 2
    vim.opt_local.concealcursor = "c"
  end,
})
