{
  config,
  lib,
  ...
}: let
  inherit (lib) singleton mkIf;
  isGraphical = config.services.xserver.enable;
in {
  home-manager.sharedModules = singleton ({pkgs, ...}: {
    home.packages = [pkgs._1password];
  });
  programs._1password.enable = true;
  programs._1password-gui = mkIf isGraphical {
    enable = true;
    # FIXME: broken if guardian user not set
    polkitPolicyOwners = [config.dotfield.guardian.username];
  };
}
