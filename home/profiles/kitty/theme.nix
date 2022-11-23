{
  config,
  self,
  ...
}: let
  inherit (self.lib.apps.kitty) makeTheme;
in {
  programs.kitty.extraConfig = makeTheme config.colorscheme.colors;

  xdg.configFile = {
    "kitty/themes".source = ./themes;

    # "kitty/themes/dark.conf".source = ./themes/dark/Substrata.conf;
    # "kitty/themes/light.conf".source = ./themes/light/Alabaster.conf;

    # FIXME: allow for specifying custom nix-colors-compatible color schemes
    # "kitty/themes/dark.conf".text = mkColorScheme theme.colors.dark;
    # "kitty/themes/light.conf".text = mkColorScheme theme.colors.light;
  };
}
