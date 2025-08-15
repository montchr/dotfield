{ lib, config, ... }:
{
  dotfield.features.workstation.nixos = {
    imports = [
      config.dotfield.features.archivist.nixos
    ];

    time.timeZone = lib.mkDefault "America/New_York";
  };

  dotfield.features.workstation.home =
    { pkgs, ... }:
    {
      imports = [
        config.dotfield.features.archivist.home
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
