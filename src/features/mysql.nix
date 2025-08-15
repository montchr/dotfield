{
  dotfield.features.mysql.nixos =
    { config, ... }:
    {
      users.groups.mysql.members = config.users.groups.wheel.members;
    };
}
