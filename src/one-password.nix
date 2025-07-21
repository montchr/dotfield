{
  flake.modules.nixos.secrets = {
    programs._1password.enable = true;
  };

  flake.modules.nixos.graphical =
    { config, pkgs, ... }:
    {
      programs._1password-gui = {
        enable = true;
        package = pkgs._1password-gui-beta;
        # package = cfg.package.override {
        #   polkitPolicyOwners = cfg.polkitPolicyOwners;
        # };
        polkitPolicyOwners = config.users.groups.wheel.members;
      };
    };
}
