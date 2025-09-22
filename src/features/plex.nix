{ lib, ... }:
{
  aspects.workstation.home =
    { pkgs, ... }:
    {
      home.packages =
        # XXX: Unsupported platform.
        lib.optionals (pkgs.stdenv.hostPlatform.system != "aarch64-linux") [ pkgs.plex-desktop ];
    };
}
