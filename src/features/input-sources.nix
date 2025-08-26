{ self, ... }:
{
  dotfield.aspects.graphical.nixos =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.wev
      ];
    };

  dotfield.aspects.workstation.nixos =
    { config, pkgs, ... }:
    {
      imports = [
        self.nixosModules."hardware/keyboardio"
      ];

      hardware.keyboard.qmk.enable = true;
      hardware.keyboard.keyboardio.enable = true;
      hardware.keyboard.zsa.enable = true;

      environment.systemPackages = [
        pkgs.wally-cli
      ];

      # Required to support flashing firmware.
      users.groups.plugdev.members = config.users.groups.wheel.members;
    };

  dotfield.aspects.graphical.home =
    { config, ... }:
    {
      home.keyboard.options = [ "compose:ralt" ];
      dconf.settings."org/gnome/desktop/input-sources" = {
        sources = [
          (config.lib.gvariant.mkTuple [
            "xkb"
            "us"
          ])
        ];
        xkb-options = [
          "terminate:ctrl_alt_bksp"
          "compose:ralt"
        ];
      };
    };
}
