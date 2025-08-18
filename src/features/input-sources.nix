{
  dotfield.features.graphical.nixos =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.wev
      ];
    };

  dotfield.features.workstation.nixos =
    { config, pkgs, ... }:
    {
      hardware.keyboard.qmk.enable = true;
      # Required to support flashing firmware.
      users.groups.plugdev.members = config.users.groups.wheel.members;
    };

  dotfield.features.graphical.home =
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
