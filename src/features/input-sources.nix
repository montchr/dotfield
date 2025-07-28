{ lib, ... }:
{
  dotfield.modules.graphical.nixos =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.wev
      ];
    };

  dotfield.modules.workstation.nixos =
    { config, pkgs, ... }:
    lib.mkMerge [
      {
        hardware.keyboard.qmk.enable = true;
      }
    ]
    # Required to support flashing firmware.
    ++ (config.lib.generateSudoersExtraGroupsModules [ "plugdev" ]);

  dotfield.modules.graphical.home =
    { config, ... }:
    {
      home.keyboard.options = [ "compose:ralt" ];
      dconf.settings."org/gnome/desktop/input-sources" = {
        sources = [
          (config.lib.hm.gvariant.mkTuple [
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
