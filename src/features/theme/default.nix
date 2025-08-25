flake@{
  self,
  lib,
  inputs,
  ...
}:
let
  inherit (self.lib.theme) toColorSchemePath;
in
{
  dotfield.aspects.workstation.nixos = {
    imports = [ flake.config.dotfield.aspects.theme.nixos ];
  };

  dotfield.aspects.theme.nixos =
    { pkgs, ... }:
    {
      imports = [ inputs.stylix.nixosModules.stylix ];

      stylix.enable = true;

      stylix.base16Scheme = lib.mkDefault (toColorSchemePath pkgs "catppuccin-mocha");
    };

  dotfield.aspects.theme.home =
    { config, pkgs, ... }:
    let
      inherit (builtins) mapAttrs removeAttrs;
      inherit (config.stylix) fonts;
      prefs = flake.config.dotfield.meta.users.${config.home.username}.preferences;
      fontFamilies = mapAttrs (_: v: removeAttrs v [ "pname" ]) prefs.theme.font.families;
    in
    {
      stylix = {
        enable = true;
        fonts = {
          inherit (prefs.theme.font) sizes;
          inherit (fontFamilies) sansSerif serif monospace;
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

}
