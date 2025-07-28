{ self, ... }:
{
  dotfield.modules.workstation.nixos = {
    imports = [
      self.dotfield.modules.archivist.nixos
    ];
  };

  dotfield.modules.workstation.home =
    { pkgs, ... }:
    {
      imports = [
        self.dotfield.modules.archivist.home
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
