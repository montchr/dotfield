{
  aspects.graphical.home =
    { pkgs, ... }:
    {
      home.packages = [
        # XXX: depends on insecure and abandoned qtwebengine-5.15.19
        # pkgs.jellyfin-media-player
      ];
    };
}
