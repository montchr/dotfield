{ self, ... }:
{
  users.cdom.aspects.graphical.home =
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
        # // (import ./__settings/ui-state.nix)
        // {
          "browser.startup.homepage" = builtins.concatStringsSep "|" [ "https://lobste.rs" ];
          "identity.fxaccounts.account.device.name" =
            hmArgs.osConfig.networking.hostName or (builtins.getEnv "HOSTNAME");
        };

      search = import ./__search.nix { inherit lib lib' pkgs; };
    in
    {
      programs.firefox.profiles.primary = {
        inherit search;
        id = 0;
        # Match the existing store ID for new profiles system compat.
        storeId = "ab9646ca";
        settings = baseSettings // { };
      };
    };
}
