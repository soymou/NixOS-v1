{ config, pkgs, vars, ... }:
{
  time.timeZone = "${vars.timeZone}";
  i18n.defaultLocale = "${vars.locale}";
  
  services.xserver.xkb = {
    layout = "${vars.keyboardLayout}";
    variant = "${vars.keyboardVariant}";
  };

  console.keyMap = "${vars.consoleKeymap}";
}
