{
  dotfield.aspects.workstation.nixos =
    { config, ... }:
    {
      programs._1password.enable = true;
      programs._1password-gui = {
        enable = true;
        polkitPolicyOwners = config.users.groups.wheel.members;
      };
    };
}
