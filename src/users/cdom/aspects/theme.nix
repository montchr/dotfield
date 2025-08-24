flake@{
  lib,
  inputs,
  ...
}:
let
  inherit (flake.config.lib.theme) toColorSchemePath;
in

{
  dotfield.users.cdom.aspects.theme.home =
    { config, pkgs, ... }:
    let
      inherit (config.stylix) fonts;
      colorScheme = prefs.theme.color.scheme.${prefs.theme.color.variant};
      prefs = flake.config.dotfield.meta.users.${config.home.username}.preferences;
    in

    {
      stylix = {
        enable = true;
        fonts = {
          inherit (prefs.theme.font) sizes;
          inherit (prefs.theme.font.families) sansSerif serif monospace;
        };
        cursor = prefs.theme.cursor;
        iconTheme = prefs.theme.icons // {
          enable = true;
        };

        # FIXME: infinite recursion why?
        # stylix.targets.firefox.profileNames = builtins.attrNames config.programs.firefox.profiles;
        # stylix.targets.vscode.profileNames = builtins.attrNames config.programs.vscode.profiles;
        targets.firefox.profileNames = [
          "home"
          "work"
        ];
        targets.vscode.profileNames = [ "default" ];
      };

      fonts.fontconfig.enable = true;
      fonts.fontconfig.defaultFonts = {
        monospace = lib.mkBefore [ config.stylix.fonts.monospace.name ];
        sansSerif = lib.mkBefore [ config.stylix.fonts.sansSerif.name ];
        serif = lib.mkBefore [ config.stylix.fonts.serif.name ];
      };

    };

  dotfield.users.cdom.aspects.theme.nixos =
    { pkgs, ... }:
    let
      toColorSchemePath' = toColorSchemePath pkgs;
    in
    {
      imports = [ inputs.stylix.nixosModules.stylix ];

      stylix.enable = true;
      stylix.base16Scheme = lib.mkDefault (toColorSchemePath' "catppuccin-mocha");

      specialisation = {
        dark.configuration = {
          stylix.base16Scheme = toColorSchemePath' "catppuccin-mocha";
        };
        light.configuration = {
          stylix.base16Scheme = toColorSchemePath' "catppuccin-latte";
        };
      };

    };
}
