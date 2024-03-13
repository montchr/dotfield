{ config, lib, ... }:
let
  isGraphical = config.services.xserver.enable;
in
{
  home-manager.sharedModules = lib.singleton (
    { pkgs, ... }:
    {
      home.packages = [ pkgs._1password ];
    }
  );

  programs._1password.enable = true;

  programs._1password-gui = lib.mkIf isGraphical {
    enable = true;
    polkitPolicyOwners = config.users.groups.wheel.members;
  };
}
