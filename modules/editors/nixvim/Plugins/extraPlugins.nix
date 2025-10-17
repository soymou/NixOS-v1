{ pkgs }: 
let 
  mdxPlugin = pkgs.vimUtils.buildVimPlugin {
    name = "mdx.nvim";
    src = pkgs.fetchFromGitHub {
      owner = "davidmh";
      repo = "mdx.nvim";
      rev = "master";
      sha256 = "sha256-jpMcrWx/Rg9sMfkQFXnIM8VB5qRuSB/70wuSh6Y5uFk=";
    };
  };
in
[
 mdxPlugin 
]
