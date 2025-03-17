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
  stylix.enable = true;
  # Inherits from the NixOS module.  Uncomment to override.
  # stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/${colorScheme}.yaml";
  stylix.fonts = {
    inherit (prefs.theme.font) sizes;
    inherit (prefs.theme.font.families) sansSerif serif monospace;
  };
  stylix.cursor = prefs.theme.cursor;
  stylix.iconTheme = prefs.theme.icons // {
    enable = true;
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
}
