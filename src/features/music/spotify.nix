{
  aspects.graphical.home =
    { lib, pkgs, ... }:
    let
      inherit (pkgs.stdenv.hostPlatform) system;
    in
    {
      home.packages = [
        pkgs.spotify-player
      ]
      # XXX: broken upstream
      ++ (lib.optional (system != "aarch64-linux") pkgs.spotify);
    };
}
