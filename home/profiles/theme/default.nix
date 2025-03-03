{
  flake,
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.stylix) fonts;
  prefs = import "${flake.self}/users/${config.home.username}/preferences.nix" {
    inherit pkgs;
  };
  colorScheme = prefs.theme.color.scheme.${prefs.theme.color.variant};
in
{
  imports = [
    ./__kitty.nix
  ];

  stylix.enable = true;
  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/${colorScheme}.yaml";
  stylix.fonts = { inherit (prefs.theme.font) sansSerif serif monospace; };
  stylix.cursor = prefs.theme.cursor;
  stylix.iconTheme = prefs.theme.icons // {
    enable = true;
  };

  fonts.fontconfig.enable = true;
  fonts.fontconfig.defaultFonts = {
    monospace = lib.mkBefore [ config.stylix.fonts.monospace.name ];
    sansSerif = lib.mkBefore [ config.stylix.fonts.sansSerif.name ];
    serif = lib.mkBefore [ config.stylix.fonts.serif.name ];
  };

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-${prefs.theme.color.variant}";
    };
    "org/gnome/desktop/wm/preferences" = {
      titlebar-uses-system-font = true;
    };
  };

  home.packages = [
    pkgs.fastfetch # another neofetch clone
  ];

  programs.ghostty.settings = {
    font-family = config.stylix.fonts.monospace.name;
    font-size = config.stylix.fonts.sizes.terminal;
    theme = colorScheme;
  };
}
