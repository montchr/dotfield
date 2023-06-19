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

    xdg.configFile = {
      "kitty/theme-dark.conf".text =
        makeConf (makeThemeAttrs
          (asHexStrings colorSchemes.dark.colors));
      "kitty/theme-light.conf".text =
        makeConf (makeThemeAttrs
          (asHexStrings colorSchemes.light.colors));
    };
  })
