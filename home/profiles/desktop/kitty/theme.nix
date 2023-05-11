{
  inputs,
  config,
  ...
}: let
  inherit (config.theme) colors fonts;
  inherit (inputs.apparat.lib.kitty) makeConf makeThemeAttrs;
  colorScheme =
    if colors.active != null
    then colors.active
    else colors.dark;
in {
  xdg.configFile = {
    "kitty/theme-dark.conf".text = makeConf (makeThemeAttrs colors.dark);
    "kitty/theme-light.conf".text = makeConf (makeThemeAttrs colors.light);
  };

  programs.kitty.settings = let
    font_family = fonts.term.family;
    font_size = fonts.term.size;
  in
    (makeThemeAttrs colorScheme)
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
