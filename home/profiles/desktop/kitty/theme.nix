{
  inputs,
  config,
  lib,
  self,
  ...
}: let
  inherit (self.lib.colors) getColorScheme;
  inherit (inputs.apparat.lib.kitty) makeConf makeThemeAttrs;
  themeCfg = config.theme;
in
  lib.mkIf themeCfg.enable {
    xdg.configFile = {
      "kitty/theme-dark.conf".text = makeConf (makeThemeAttrs (getColorScheme themeCfg.colors.dark));
      "kitty/theme-light.conf".text = makeConf (makeThemeAttrs (getColorScheme themeCfg.colors.light));
    };

    programs.kitty.settings = let
      font_family = themeCfg.fonts.term.family;
      font_size = themeCfg.fonts.term.size;
    in
      (makeThemeAttrs (getColorScheme themeCfg.colors.active))
      // {
        inherit font_family font_size;
        bold_font = "${font_family} Bold";
        italic_font = "${font_family} Italic";
        bold_italic_font = "${font_family} Bold Italic";
      };

    # FIXME: the weight suffixes are different between fonts
    # FIXME: include this in the theme module itself, as these are properties of
    #        the font, not specific to kitty or any other application
    # $ kitty +list-fonts --psnames | grep <font-name>
    programs.kitty.extraConfig = let
      psName = "BerkeleyMono";
    in ''
      font_features ${psName}             -calt +dlig
      font_features ${psName}-Bold        -calt +dlig
      font_features ${psName}-Italic      -calt +dlig
      font_features ${psName}-BoldItalic  -calt +dlig
    '';
  }
