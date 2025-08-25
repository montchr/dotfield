{
  dotfield.aspects.graphical.nixos =
    { config, ... }:
    {
      networking.networkmanager.enable = true;
      users.groups.networkmanager.members = config.users.groups.wheel.members;
    };
}
