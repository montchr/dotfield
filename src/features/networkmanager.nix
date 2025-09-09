{
  aspects.graphical.nixos =
    { config, ... }:
    {
      networking.networkmanager.enable = true;
      users.groups.networkmanager = { inherit (config.users.groups.wheel) members; };
    };
}
