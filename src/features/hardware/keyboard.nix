{
  aspects.workstation.nixos =
    { config, pkgs, ... }:
    {
      # Required to support flashing firmware.
      users.groups.plugdev = { inherit (config.users.groups.wheel) members; };

      hardware.keyboard.keyboardio.enable = true;
      hardware.keyboard.qmk.enable = true;
      hardware.keyboard.zsa.enable = true;

      environment.systemPackages = [
        pkgs.wally-cli
      ];
    };
}
