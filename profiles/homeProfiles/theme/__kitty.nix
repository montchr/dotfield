{
  flake,
  config,
  lib,
  ...
}: let
  inherit (flake.inputs.apparat.lib.kitty) makeConf makeThemeAttrs;
  inherit (flake.self.lib.theme) asHexStrings;
  inherit (config) theme;
  cfg = config.programs.kitty;
in
  lib.mkIf cfg.enable (let
    colorSchemes = theme.color.schemes;
    colorSettings = makeThemeAttrs (asHexStrings colorSchemes.default.colors);

    fontSettings = let
      font_family = theme.fonts.terminal.name;
      font_size = theme.fonts.terminal.size;
    in {
      inherit font_family font_size;
      bold_font = "${font_family} Bold";
      italic_font = "${font_family} Italic";
      bold_italic_font = "${font_family} Bold Italic";
    };
  in {
    programs.kitty.settings = colorSettings // fontSettings;
    # FIXME: should not hard-code non-default + proprietary font
    #        consider some lib mapping of known fonts to postscript names
    programs.kitty.extraConfig = let
      # $ kitty +list-fonts --psnames | grep <font-name>
      psName = "BerkeleyMono";
    in ''
      font_features ${psName}             -calt +dlig
      font_features ${psName}-Bold        -calt +dlig
      font_features ${psName}-Italic      -calt +dlig
      font_features ${psName}-BoldItalic  -calt +dlig
    '';

    xdg.configFile = {
      "kitty/theme-dark.conf".text =
        makeConf (makeThemeAttrs
          (asHexStrings colorSchemes.dark.colors));
      "kitty/theme-light.conf".text =
        makeConf (makeThemeAttrs
          (asHexStrings colorSchemes.light.colors));
    };
  })
