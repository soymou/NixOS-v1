# Add your reusable home-manager modules to this directory, on their own file (https://nixos.wiki/wiki/Module).
# These should be stuff you would like to share with others, not your personal configurations.
{
  illogical-impulse = import ./ui/illogical-impulse;
  nixvim = import ./editors/nixvim;
  zen-browser = import ./browsers/zen-browser;
  minecraft = import ./misc/minecraft;
  emacs = import ./editors/emacs;
}
