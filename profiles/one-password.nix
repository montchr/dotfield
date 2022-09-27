{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (lib) mkIf mkMerge optional;
in
  mkMerge [
    {
      home-manager.sharedModules = [
        ({pkgs, ...}: {
          home.packages =
            [pkgs._1password]
            ++ (optional (!isDarwin) pkgs._1password-gui);
        })
      ];
    }
    (mkIf (config.programs ? _1password) {programs._1password.enable = true;})
    (mkIf (config.programs ? _1password-gui) {
      programs._1password-gui.enable = true;
      programs._1password-gui.polkitPolicyOwners = [config.dotfield.guardian.username];
    })
  ]
