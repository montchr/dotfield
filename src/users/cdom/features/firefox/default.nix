{ self, ... }:
{
  aspects.graphical.home = # TODO: do theme stuff in theme profile / only if theme enabled
    hmArgs@{
      config,
      pkgs,
      lib,
      ...
    }:
    let
      inherit (pkgs.stdenv.hostPlatform) isLinux;
      lib' = self.lib;

      cfg = config.programs.firefox;

      baseSettings =
        (import ./__settings/common.nix)
        // (import ./__settings/browser-toolbox.nix)
        // (import ./__settings/ui-state.nix)
        // {
          "browser.startup.homepage" = builtins.concatStringsSep "|" [ "https://lobste.rs" ];
          "identity.fxaccounts.account.device.name" =
            hmArgs.osConfig.networking.hostName or (builtins.getEnv "HOSTNAME");
        };

      userChrome = builtins.readFile ./userChrome.css;

      search = import ./__search.nix { inherit lib lib' pkgs; };
    in
    {
      programs.firefox.profiles.home = {
        inherit
          search
          userChrome
          ;
        id = 0;
        settings = baseSettings // { };
      };

      programs.firefox.profiles.work = {
        inherit search;
        id = 1;
        settings = baseSettings // {
          "browser.startup.homepage" = "about:blank";
          "userChrome.theme.monospace" = false;
        };
      };

      # FIXME: the generic Firefox desktop item does not open the `home` profile by
      # default, hence the definition of the additional `firefox-home-profile`
      home.packages = lib.optionals isLinux [
        # TODO: make this an optional package for per-machine usage
        (pkgs.makeDesktopItem {
          name = "firefox-work-profile";
          desktopName = "Firefox (Work)";
          genericName = "Open a Firefox window scoped to the Work profile.";
          icon = "firefox";
          exec = "${cfg.package}/bin/firefox -P ${cfg.profiles.work.path}";
          categories = [
            "Application"
            "Network"
            "WebBrowser"
          ];
        })
        (pkgs.makeDesktopItem {
          name = "firefox-home-profile";
          desktopName = "Firefox (Home)";
          genericName = "Open a Firefox window scoped to the Home profile.";
          icon = "firefox";
          exec = "${cfg.package}/bin/firefox -P ${cfg.profiles.home.path}";
          categories = [
            "Application"
            "Network"
            "WebBrowser"
          ];
        })
      ];

      # TODO: extract to function
      # home.file = l.mkMerge (l.flip l.mapAttrsToList cfg.profiles (_: profile: let
      #   profileDir = "${profilesPath}/${profile.path}";
      # in {
      # }));
    };
}
