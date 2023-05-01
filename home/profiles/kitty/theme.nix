{
  inputs,
  config,
  ...
}: let
  inherit (config) theme;
  inherit (inputs.apparat.lib.kitty) makeConf makeThemeAttrs;
  colorScheme =
    if theme.colors.active != null
    then theme.colors.active
    else theme.colors.dark;
in {
  programs.kitty.settings = makeThemeAttrs colorScheme;

  xdg.configFile = {
    "kitty/theme-dark.conf".text = makeConf (makeThemeAttrs theme.colors.dark);
    "kitty/theme-light.conf".text = makeConf (makeThemeAttrs theme.colors.light);
  };
}
