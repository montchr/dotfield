{
  dotfield.features.workstation.nixos =
    { config, ... }:
    {
      services.printing.enable = true;
      hardware.sane = {
        enable = true;
        openFirewall = true;
      };

      users.groups.cups.members = config.users.groups.wheel.members;
      users.groups.lp.members = config.users.groups.wheel.members;
      users.groups.scanner.members = config.users.groups.wheel.members;
    };
}
