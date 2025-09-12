{
  aspects.workstation.nixos =
    { config, ... }:
    {
      services.printing.enable = true;
      services.avahi.openFirewall = true;

      # scanner support
      hardware.sane = {
        enable = true;
        openFirewall = true;
      };

      users.groups.cups = { inherit (config.users.groups.wheel) members; };
      users.groups.lp = { inherit (config.users.groups.wheel) members; };
      users.groups.scanner = { inherit (config.users.groups.wheel) members; };
    };
}
