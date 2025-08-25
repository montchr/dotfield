{
  dotfield.aspects.workstation.nixos =
    { config, ... }:
    {
      users.groups.plugdev.members = config.users.groups.wheel.members;
    };
}
