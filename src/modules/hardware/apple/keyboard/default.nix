{
  dotfield.modules.workstation =
    { pkgs, ... }:
    {
      # Required to support flashing firmware.
      dotfield.guardian.extraGroups = [ "plugdev" ];

      hardware.keyboard.qmk.enable = true;

      hardware.keyboard.keyboardio.enable = true;

      hardware.keyboard.zsa.enable = true;
      environment.systemPackages = [
        pkgs.keymapp
        pkgs.wally-cli
      ];
    };
}
