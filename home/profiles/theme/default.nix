{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.stylix) fonts;
in
{
  imports = [
    ./__kitty.nix

  ];

  # theme.enable = true;
  stylix.enable = true;
  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-latte.yaml";
  stylix.fonts.sansSerif = {
    name = "Inter";
    package = pkgs.inter;
  };
  stylix.fonts.serif = {
    name = "Aporetic Serif";
    package = pkgs.aporetic;
  };
  stylix.fonts.monospace = {
    name = "Aporetic Sans Mono";
    package = pkgs.aporetic;
  };

  fonts.fontconfig.enable = true;
  fonts.fontconfig.defaultFonts = {
    monospace = lib.mkBefore [ config.stylix.fonts.monospace.name ];
    sansSerif = lib.mkBefore [ config.stylix.fonts.sansSerif.name ];
    serif = lib.mkBefore [ config.stylix.fonts.serif.name ];
  };

  dconf.settings = {
    "org/gnome/desktop/wm/preferences" = {
      titlebar-uses-system-font = true;
    };
  };

  home.packages = [
    pkgs.fastfetch # another neofetch clone

    config.stylix.fonts.monospace.package
    config.stylix.fonts.sansSerif.package
    config.stylix.fonts.serif.package
  ];

  # home.pointerCursor = {
  #   name = "Adwaita";
  #   package = pkgs.gnome-themes-extra;
  # };

  #  programs.bat.config.theme = "base16";
  # programs.git.delta.options.features = "base16";
  # programs.git.delta.options.light = cfg.color.scheme.default.kind == "light";
  # programs.git.difftastic.background = reversePolarity cfg.theme.color.scheme.default.kind;
  programs.ghostty.settings = {
    font-family = config.stylix.fonts.monospace.name;
    font-size = 11;
    theme = "dark:catppuccin-mocha,light:catppuccin-latte";
  };
}
