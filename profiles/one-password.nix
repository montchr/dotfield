{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) hasAttr;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (lib) mkMerge optionals optionalAttrs;
in
  mkMerge [
    {
      home-manager.sharedModules = [
        ({pkgs, ...}: {
          home.packages =
            [pkgs._1password]
            ++ (optionals (!isDarwin) pkgs._1password-gui);
        })
      ];
    }
    (optionalAttrs (hasAttr "_1password" options.programs) {programs._1password.enable = true;})
    (optionalAttrs (hasAttr "_1password-gui" options.programs) {
      programs._1password-gui.enable = true;
      programs._1password-gui.polkitPolicyOwners = [config.dotfield.guardian.username];
    })
  ]
