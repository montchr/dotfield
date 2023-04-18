{
  inputs,
  config,
  ...
}: let
  inherit (config) theme;
  inherit (inputs.apparat.lib.apps.kitty) makeThemeAttrs;
  colorScheme =
    if theme.colors.active != null
    then theme.colors.active
    else theme.colors.dark;
in {
  programs.kitty.settings = makeThemeAttrs colorScheme;

  xdg.configFile = {
    "kitty/theme-dark.conf".source = ./themes/alabaster-dark.conf;
    "kitty/theme-light.conf".source = ./themes/alabaster.conf;
  };
}
