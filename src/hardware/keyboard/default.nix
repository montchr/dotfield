{
  flake.modules.nixos.workstation =
    { pkgs, ... }:
    {
      hardware.keyboard.qmk.enable = true;

      hardware.keyboard.keyboardio.enable = true;

      hardware.keyboard.zsa.enable = true;
      environment.systemPackages = [
        pkgs.keymapp
        pkgs.wally-cli
      ];
    };

  flake.modules.nixos.admin-user = {
    # Required to support flashing firmware.
    dotfield.guardian.extraGroups = [ "plugdev" ];
  };
}
