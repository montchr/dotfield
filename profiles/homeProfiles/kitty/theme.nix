{
  flake,
  config,
  ...
}: let
  inherit (flake.inputs.apparat.lib.kitty) makeConf makeThemeAttrs;
  inherit (flake.self.lib.theme) asHexStrings;
  colorSchemes = config.theme.color.schemes;
in {
  programs.kitty.settings =
    makeThemeAttrs
    (asHexStrings colorSchemes.default.colors);

  xdg.configFile = {
    "kitty/theme-dark.conf".text =
      makeConf (makeThemeAttrs
        (asHexStrings colorSchemes.dark.colors));
    "kitty/theme-light.conf".text =
      makeConf (makeThemeAttrs
        (asHexStrings colorSchemes.light.colors));
  };
}
