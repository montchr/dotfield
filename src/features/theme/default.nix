{
  lib,
  self,
  inputs,
  ...
}:
let
  inherit (self.lib.theme) toColorSchemePath;
in

{
  dotfield.features.theme.home =
    { config, pkgs, ... }:
    let
      inherit (config.stylix) fonts;
      colorScheme = prefs.theme.color.scheme.${prefs.theme.color.variant};
      prefs = self.dotfield.meta.users.${config.home.username}.preferences;
    in

    {
      stylix = {
        enable = true;
        fonts = {
          inherit (prefs.theme.font) sizes;
          inherit (prefs.theme.font.families) sansSerif serif monospace;
        };
        # FIXME: set sensible defaults, not weird shit like my config
        # cursor = prefs.theme.cursor;
        # iconTheme = prefs.theme.icons // {
        #   enable = true;
        # };
      };

      fonts.fontconfig.enable = true;
      fonts.fontconfig.defaultFonts = {
        # FIXME: does lib.mkBefore prevent lib.mkForce?
        monospace = lib.mkBefore [ config.stylix.fonts.monospace.name ];
        sansSerif = lib.mkBefore [ config.stylix.fonts.sansSerif.name ];
        serif = lib.mkBefore [ config.stylix.fonts.serif.name ];
      };

      dconf.settings."org/gnome/desktop/wm/preferences" = {
        titlebar-uses-system-font = true;
      };

      home.packages = [
        pkgs.fastfetch
      ];
    };

  dotfield.features.theme.nixos =
    { pkgs, ... }:
    {
      imports = [ inputs.stylix.nixosModules.stylix ];

      stylix.enable = true;
    };
}
