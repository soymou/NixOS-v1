{ ... }:
{
  programs.nyxt = {
    enable = true;
    config = builtins.readFile ./config/config.lisp;
  };
}
