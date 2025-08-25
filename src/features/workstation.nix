{ lib, config, ... }:
let
  inherit (builtins) map;
in
{
  dotfield.aspects.workstation.nixos = {
    imports =
      (with config.dotfield.aspects; [
        archivist
        audio
        graphical
      ])
      |> map (v: v.nixos);

    time.timeZone = lib.mkDefault "America/New_York";
  };

  dotfield.aspects.workstation.home =
    { pkgs, ... }:
    {
      imports =
        (with config.dotfield.aspects; [
          archivist
          audio
          graphical
        ])
        |> map (v: v.home);

      home.packages = [
        pkgs.dex # helper for working with xdg desktop entries
        pkgs.mediainfo
        pkgs.thunderbird-latest # mail client
        pkgs.varia # download manager with torrent support
        pkgs.ydotool # command-line automation tool
      ];
    };
}
