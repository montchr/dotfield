flake@{ ... }:
{
  users.cdom.aspects.graphical.home =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      inherit (config.stylix) fonts;
      prefs = flake.config.meta.users.${config.home.username}.preferences;
      colorScheme = prefs.theme.color.scheme.${prefs.theme.color.variant};
    in
    {
      stylix.enable = true;
      stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/${colorScheme}.yaml";

      stylix.fonts = {
        inherit (prefs.theme.font) sizes;
      }
      // (lib.mapAttrs (_: v: {
        inherit (v) name;
        package = pkgs.${v.pname};
      }) prefs.theme.font.families);

      stylix.cursor = {
        inherit (prefs.theme.cursor) name size;
        package = pkgs.${prefs.theme.cursor.pname};
      };

      stylix.iconTheme = {
        inherit (prefs.theme.icons) dark light;
        enable = true;
        package = pkgs.${prefs.theme.icons.pname};
      };

      stylix.targets.floorp.enable = false;
      stylix.targets.librewolf.enable = false;
      stylix.targets.firefox.profileNames = [
        "home"
        "work"
      ];
      stylix.targets.vscode.profileNames = [ "default" ];
      # FIXME: infinite recursion why?
      # stylix.targets.firefox.profileNames = builtins.attrNames config.programs.firefox.profiles;
      # stylix.targets.vscode.profileNames = builtins.attrNames config.programs.vscode.profiles;

      fonts.fontconfig.enable = true;
      fonts.fontconfig.defaultFonts = {
        monospace = lib.mkBefore [ config.stylix.fonts.monospace.name ];
        sansSerif = lib.mkBefore [ config.stylix.fonts.sansSerif.name ];
        serif = lib.mkBefore [ config.stylix.fonts.serif.name ];
      };

      dconf.settings = {
        "org/gnome/desktop/interface" = {
          # HACK: override stylix -- why does it set 'default'?
          # color-scheme = lib.mkForce "prefer-${prefs.theme.color.variant}";
        };
        "org/gnome/desktop/wm/preferences" = {
          titlebar-uses-system-font = true;
        };
      };

      home.packages = [
        pkgs.fastfetch # another neofetch clone
      ];
    };
}
