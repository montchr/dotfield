{ moduleWithSystem, ... }:
{
  users.cdom.aspects.graphical.home = moduleWithSystem (
    perSystem@{ config }:
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      colorScheme = "catppuccin-mocha";
    in
    {
      stylix.enable = true;
      stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/${colorScheme}.yaml";

      stylix.fonts = {
        sizes = {
          applications = 10;
          desktop = 10;
          popups = 8;
          terminal = 10;
        };

        sansSerif = {
          name = "Inter";
          package = pkgs.inter;
          # name = "Berkeley Mono";
          # package = perSystem.config.packages.berkeley-mono;
        };

        serif = {
          name = "Aporetic Serif";
          package = pkgs.aporetic;
        };

        monospace = {
          name = "Aporetic Sans Mono";
          package = pkgs.aporetic;
        };
      };

      # alternatively: posy-cursors / graphite-cursors / vanilla-dmz /
      # catppuccin-cursors / hackneyed-x11-cursors / openzone-cursors
      stylix.cursor = {
        # name = "phinger-cursors-dark";
        # package = pkgs.phinger-cursors;
        # size = 24;
        # name = "Posy_Cursor_Black";
        # package = pkgs.posy-cursors;
        # size = 32;
        name = "Bibata-Modern-Classic";
        package = pkgs.bibata-cursors;
        size = 10;
      };

      stylix.iconTheme = {
        enable = true;
        package = pkgs.papirus-icon-theme;
        dark = "Papirus Dark";
        light = "Papirus Light";
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

      # TODO: move this to common aspect
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
  );
}
