{ pkgs, config, ... }:
let
  cfg = config.programs._1password-gui;
in
{
  programs._1password.enable = true;

  programs._1password-gui = {
    enable = true;
    package = pkgs._1password-gui-beta;
    # package = cfg.package.override {
    #   polkitPolicyOwners = cfg.polkitPolicyOwners;
    # };
    polkitPolicyOwners = config.users.groups.wheel.members;
  };
}
