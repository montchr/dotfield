{ lib, config, ... }:
{
  dotfield.aspects.workstation.nixos = {
    imports = with config.dotfield.aspects; [
      archivist.nixos
      graphical.nixos
      networkmanager.nixos
    ];

    time.timeZone = lib.mkDefault "America/New_York";
  };

  dotfield.aspects.workstation.home =
    { pkgs, ... }:
    {
      imports = [
        config.dotfield.aspects.archivist.home
        config.dotfield.aspects.graphical.home
      ];

      home.packages = [
        pkgs.dex # helper for working with xdg desktop entries
        pkgs.mediainfo
        pkgs.thunderbird-latest # mail client
        pkgs.varia # download manager with torrent support
        pkgs.ydotool # command-line automation tool
      ];
    };
}
